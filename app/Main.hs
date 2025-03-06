{-# LANGUAGE MultiWayIf, ViewPatterns #-}

module Main where

--Brick imports.
import Brick.Main
    ( customMainWithDefaultVty
    , App(App)
    )
import Brick.Types
    ( Widget
    , EventM
    , BrickEvent
        ( VtyEvent
        , AppEvent
        )
    , get
    , put
    , modify
    )
import Brick.Widgets.Core
    ( str
    , emptyWidget
    , (<=>)
    , (<+>)
    )
import Brick.AttrMap (forceAttrMap, AttrMap)
import Brick.BChan
    ( BChan
    , newBChan
    , writeBChan
    , readBChan
    )

--Vty imports.
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events
    ( Event(EvKey)
    , Key(..)
    , Modifier(..)
    )

--Base imports.
import Control.Monad.IO.Class (liftIO)
import Control.Monad ( forever )
import System.Random qualified as Rand
import System.Exit
    ( exitWith
    , ExitCode (ExitSuccess)
    )
import Data.Foldable (foldl')
import Data.Monoid (Endo(Endo), appEndo)
import Control.Concurrent (forkIO, threadDelay)

--Vector imports.
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as Vector.Mutable

--Hard-coded constants.
gameDimensions :: (Int, Int)
gameDimensions = (30,30)

{-| Game state datatype. -}
data BrickGame = BrickGame
    { gameState        :: Vector.Vector (Vector.Vector Bool)
    , cursor           :: (Int, Int)
    , blockType        :: BlockType
    , blockOrientation :: Direction
    , rndGen           :: Rand.StdGen
    }
    deriving (Eq, Show)

{-| Generator of default value for game datatype. -}
initialBrickGame :: Rand.StdGen -> BrickGame
initialBrickGame = BrickGame
    ( Vector.replicate
        (snd gameDimensions)
        (Vector.replicate
            (fst gameDimensions)
            False
        )
    )
    (0,0)
    Box
    Up

{-| Block type, uses Enum for generation from random generator. -}
data BlockType
    = Box
    | LeftL
    | RightL
    | LightningUp
    | LightningDown
    | Line
    | Prod
    deriving (Eq, Show, Enum)

{-| Direction objects. Use Main or module prefix to disambiguate
  from Either type constructors. -}
data Direction
    = Up
    | Left
    | Right
    | Down
    deriving (Eq, Show)

{-| Provide stdGen for initial data, then call Brick. -}
main :: IO ()
main = do
    stdGen <- Rand.initStdGen
    bChannel <- newBChan 5
    forkIO . forever $ do
        writeBChan bChannel ()
        threadDelay 250_000
    customMainWithDefaultVty
          (Just bChannel)
	  app 
	$ initialBrickGame stdGen
    pure ()

{-| App package -}
app :: App BrickGame () ()
app = App
    drawer
    (\_ _ -> Nothing)
    eventHandler
    appInit
    attrMap

{-| Drawing function. -}
drawer :: BrickGame -> [Widget ()]
drawer game = pure . foldl' (<=>) emptyWidget $ do
    let cursor = makeCursor game
        yMax = (Vector.length $ gameState game) - 1
    y <- [yMax, yMax-1..0]
    pure $ foldl' (<+>) emptyWidget
        $ do x <- [0..(Vector.head . fmap Vector.length $ gameState game) - 1]
             pure $ if | gameState game Vector.! y Vector.! x -> Main.x
                       | (x,y) `elem` cursor -> o
                       | otherwise -> blank

{-| Generates grid numbers for where a cursor should be drawn.-}
makeCursor :: BrickGame -> Vector.Vector (Int, Int)
makeCursor game = realize $ rotate (blockOrientation game) shape
  where
    realize = let (x,y) = cursor game in
        fmap (\(u,v) -> (u + x, v + y))
    shape = Vector.fromList $ case blockType game of
        Box -> [(0,0), (0,-1), (1,0), (1,-1)]
        LeftL -> [(0,1), (0,0), (0,-1), (1,-1)]
        RightL -> [(0,1), (0,0), (-1,-1), (0,-1)]
        LightningUp -> [(1,1), (0,0), (1,0), (0,-1)]
        LightningDown -> [(0,1), (0,0), (1,0), (1,-1)]
        Line -> [(0, 1), (0, 0), (0,-1), (0,-2)]
        Prod -> [(0, 1), (-1, 0), (0,0), (1,0)]

{-| Helper for makeCursor to provide the correct rotation for a block. -}
rotate :: Direction -> Vector.Vector (Int, Int) -> Vector.Vector (Int, Int)
rotate direction = fmap (\(x,y) -> case direction of
    Up         -> (x,y)
    Main.Right -> (y,-x)
    Down       -> (-x,-y)
    Main.Left  -> (-y,x)
    )

-- Tile widgets, could possibly upgrade to images later.
x,o :: Widget ()
x = str "x"
o = str "o"
blank = str " "

{-| Event handler.-}
eventHandler :: BrickEvent () () -> EventM () BrickGame ()
eventHandler event = case event of
    VtyEvent (EvKey key mods)
        | and [KChar 'c' == key, [MCtrl] == mods ] -> quit
        | key `elem` [KUp, KLeft, KRight, KDown] -> processDirection key
    AppEvent () -> processDirection KDown
    _ -> pure ()

{-| Core state changer on inputs. -}
processDirection :: Key -> EventM () BrickGame ()
processDirection key = do
    game <- get

    let shift (x,y) = case key of
            KLeft  -> (x - 1, y)
            KRight -> (x + 1, y)
            KDown  -> (x, y - 1)
	rotated = case blockOrientation game of
	    Up -> Main.Right
	    Main.Right -> Down
	    Down -> Main.Left
	    Main.Left -> Up
        candidateGame = case key of
	    KUp -> game {blockOrientation = rotated}
	    _  -> game {cursor = shift $ cursor game}

    let validMove = not . or $ ($ candidateGame)
           <$> [isCursorOutOfBounds, isCursorOverlapping]
    
    if | validMove -> put candidateGame
       | KDown == key -> adhereCursorToBlocks
       | otherwise -> pure ()

{-| Checks whether cursor's blocks are outside of
 -  the game board. Creates annoying issue wherein
 -  I can't move while part of the cursor is out of bounds
 -  via being above the screen. -}
isCursorOutOfBounds :: BrickGame -> Bool
isCursorOutOfBounds game = or $ checkConds <$> candidateCursor
  where
    checkConds (x,y) = or
        [ x < 0
        , x >= length (Vector.head $ gameState game)
        , y < 0
        , y >= length (gameState game)
        ]
    candidateCursor = makeCursor game

{-| Checks whether cursor's blocks are over the top,
 - and is needed to trigger game overs. -}
isCursorOverTheTop :: BrickGame -> Bool
isCursorOverTheTop game = or
      $ (\(_,y) -> y >= length (gameState game))
    <$> makeCursor game

{-| Checks whether the cursor's new location would
 - overlap with existing blocks on the board.
 - Used in conjunction with key down to trigger
 - block adhesion to board. -}
isCursorOverlapping :: BrickGame -> Bool
isCursorOverlapping game = or $ do
    (x,y) <- makeCursor game
    pure $ gameState game Vector.! y Vector.! x

{-| Block adhesion to board function. Also responsible
 - for triggering game overs. -}
adhereCursorToBlocks :: EventM () BrickGame ()
adhereCursorToBlocks = do
    game <- get

    if isCursorOverTheTop game
        then gameOver
        else do
            let realizedCursor = makeCursor game

	        -- don't ask, this is how the Vector design is supposed to allow you to
	        -- modify a vector of vectors. Maybe this is just strongly discouraged?
                writeToGame (x,y) = Vector.modify
                    (\board -> Vector.Mutable.modify board
		        (Vector.modify
		            (\slice -> Vector.Mutable.write
                                slice 
                                x 
                                True
		            )
                        )
                        y
		    )
         
                affixedBlocks = appEndo
                    (foldMap (Endo . writeToGame) realizedCursor)
                    $ gameState game

                filteredBlocks = Vector.filter (not . Vector.and) affixedBlocks

                newBoard = filteredBlocks
	            <> Vector.replicate (snd gameDimensions - length filteredBlocks)
		       (Vector.replicate (fst gameDimensions) False)

	        updatedGame = game {gameState = newBoard}

            put updatedGame
            newBlock
            resetCursor

{-| Game over. PutStrLn doesn't actually display,
 - because Brick aggressively messes with stdout and stderr. -}
gameOver :: EventM () BrickGame ()
gameOver = do
    liftIO $ putStrLn "You lose!"
    quit

{-| Quit used when the Ctrl + C keypress is triggered. -}
quit :: EventM () BrickGame ()
quit = liftIO $ exitWith ExitSuccess

{-| Randomizes block on init, and resets the cursor. -}
appInit :: EventM () BrickGame ()
appInit = do
    newBlock
    resetCursor

{-| Creates new block after adhesion occurs. -}
newBlock :: EventM () BrickGame ()
newBlock = do
    game <- get
    let (count, newGen) = Rand.uniformR (0,6) $ rndGen game
    modify (\game -> game
        { rndGen = newGen
        , blockType = toEnum count
        , blockOrientation = Up
        }
        )

{-| Resets cursor. -}
resetCursor :: EventM () BrickGame ()
resetCursor = do
    modify (\game@BrickGame{cursor} -> game
        {cursor =
            ( fst gameDimensions `div` 2
            , snd gameDimensions - 1
            )
        } )

{-| Dummy item used to get the game to render without
 - messing with options. -}
attrMap :: BrickGame -> AttrMap
attrMap _ = forceAttrMap defAttr

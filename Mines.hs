import System.Random
import System.Random.Shuffle (shuffle')
import Data.Map as Map
import Data.Set as Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
import System.IO
-- import Data.Either.Unwrap
--import Prelude (foldr, filter, map) -- avoid name clash


-- game entry point
main :: IO ()
main = do 
        putStr $ "Choose difficulty : \n"
            ++ "Type '1' for (Beginner) \n"
            ++ "Type '2' for Intermediate \n"
            ++ "Type '3' for Expert \n"
        hFlush stdout                           -- make sure the string is flushed
        m <- getLine                            -- read the users input
        generator <- getStdGen                  -- get a random number generator
        let mode = getMode m                    -- convert string to a Mode object
        putStr ((show mode) ++ " is chosen.")   -- inform the user about the mode
        hFlush stdout
        play (InWindow "2IPH0" (getWindow $ playgroundSize mode) (300, 300)) -- window settings
            white                               -- background color
            50                                  -- update rate
            (getInitialModel generator mode)    -- initial state of the game
            view                                -- graphics render
            actionHandler                       -- click/press listener
            stub                        
            where 
                stub _ = id
                getWindow c = applyBoth (* (round cellSize)) c -- cellSize is hardcoded

---------------------------------
--          Constants          --
---------------------------------

cellSize :: Float
cellSize = 30

---------------------------------
--       Helper functions      --
---------------------------------

-- simply shuffle cells of the grid
mineShuffle :: RandomGen g => g -> Mode -> [Cell] -> [Cell]
mineShuffle g m l = shuffle' l (h * w  - 1) g
    where
        w = fst $ playgroundSize m
        h = snd $ playgroundSize m

-- applies function to each element of the pair
applyBoth :: (a -> b) -> (a, a) -> (b, b)
applyBoth g pair = (g $ fst pair, g $ snd pair)

-- Generate random mines, such that none of them is equal to the cell
-- that the user initially clicked on
generateMines :: RandomGen g => g -> Cell -> Mode -> Mines
generateMines rndGen initialCell mode = Set.fromList 
    $ take mines
    $ mineShuffle rndGen mode 
    $ [(a, b) | a <- [0 .. x - 1] , b <- [0 .. y - 1], not $ (a, b) == initialCell]
        where
            x = fst $ playgroundSize mode
            y = snd $ playgroundSize mode
            mines = minesNr mode               

-- checks if the cells within the field is a mine
isMine :: Cell -> Field -> Bool
isMine c f = case Map.lookup c f of
    (Just Mine) -> True
    _           -> False

-- add the cell to a map
addCell :: Cell -> CellState -> Field -> Field
addCell cell state field = Map.insert cell state field

-- maps a cell to the screen
mapCell :: (Int, Int) -> (Float, Float)
mapCell = applyBoth ((* cellSize) . fromIntegral)

-- convert a string to a Model object
getMode :: String -> Mode
getMode "2"   =  Mode "Intermediate"(15, 15) 40
getMode "3"   =  Mode "Expert" (30, 15) 99
getMode _     =  Mode "Beginner" (10, 10) 10

-- get the initial state of the game
getInitialModel :: StdGen -> Mode -> GameState
getInitialModel g m = GameState (Map.fromList []) (Left g) m False 

-- maps the screen to a cell
mapScreen :: (Float, Float) -> (Int, Int) -> (Int, Int)
mapScreen screen grid = applyBoth (round . (/ cellSize)) (invertViewPort (viewPort grid) screen)

-- translates the grid properly
viewPort :: (Int, Int) -> ViewPort
viewPort (x, y) = ViewPort (applyBoth (negate . (/ 2) . (subtract cellSize)) $ mapCell (x, y)) 0 1

---------------------------------
--           Types             --
---------------------------------

-- the playground
type Field = Map Cell CellState

-- the mode chosen by the user. it influences a certain number of cells and mines.
data Mode = Mode 
    {
        name                :: [Char],
        playgroundSize      :: (Int, Int),
        minesNr             :: Int
    } deriving Show

-- a cell
type Cell = (Int, Int)

-- a collection of mines
type Mines = Set Cell

-- saves state of the cell, it can be :
--      explored (after the user clicked on it), 
--            we also display the number of mines around this cell (Checked)
--      there is a mine hidden (Mine)
--      user flagged it (Flag)
data CellState = Checked Int | Mine | Flag

-- keeps the state of the game
data GameState = GameState 
    { 
        field   :: Field,
        mines   :: Either StdGen Mines,
        mode    :: Mode, 
        isOver  :: Bool
    }

---------------------------------
-- Define behaviour of the GUI --
---------------------------------

-- defines logic to render GUI
view :: GameState -> Picture
view GameState { field = fld, mode = mode } 
    = applyViewPortToPicture (viewPort gridSize) $ pictures [cells, grid, logos] --pictures $ [cells, grid] 
    where
        gridSize = playgroundSize mode

        -- creates a wireframed rectangle than defines a unit of a grid
        drawGrid :: Picture
        drawGrid = rectangleWire cellSize cellSize

        -- creates a solid rectangle that defines a single cell
        drawCell :: Picture
        drawCell = rectangleSolid cellSize cellSize

        -- generates a list of grid coordinates
        generateRangeToScreen :: [(Int, Int)]
        generateRangeToScreen = [(x, y) | x <- [0 .. (fst gridSize) - 1], y <- [0 .. (snd gridSize) - 1]]

        -- combines properties of a picture into one complete image 
        combine :: (Int, Int) -> Color -> Picture -> Picture
        combine cell clr fig = translate x y $ color clr $ fig
            where
                x = fst $ mapCell cell
                y = snd $ mapCell cell

        grid :: Picture
        grid = pictures [ combine c black drawGrid | c <- generateRangeToScreen ]
        cells :: Picture
        cells = pictures [ combine c (colorCell $ Map.lookup c fld) drawCell | c <- generateRangeToScreen ]
            where 
                colorCell :: Maybe CellState ->  Color
                colorCell Nothing               = white
                colorCell (Just Mine)           = red
                colorCell (Just (Checked val))  = green
                colorCell _                     = yellow
        
        logos = pictures [ combine c black $ place $ text (tagCell $ Map.lookup c fld) | c <- generateRangeToScreen ]
            where
                tagCell :: Maybe CellState -> [Char]
                tagCell (Just Mine)           = ";("
                tagCell (Just (Checked val))  = show val
                tagCell (Just Flag)           = "F"
                tagCell _                     = ""

                place = translate (-5) (-5) . scale 0.1 0.1 . color black
                
---------------------------------
--        Events handlers      --
---------------------------------
actionHandler :: Event -> GameState -> GameState
-- left mouse click action
actionHandler (EventKey (MouseButton LeftButton) Down _ mouse) gameState =
    case gameState of
        GameState { mines = (Left m), field = fld, mode = mode } -> GameState 
            { 
                mines = Right (generateMines m (mapScreen mouse gridSize) mode),
                isOver = False,
                field = fld,
                mode = mode
            }
                where 
                    gridSize = playgroundSize mode
        GameState { mines = (Right m), field = fld, isOver = False, mode = mode } -> 
            GameState
                {
                    field = event (mapScreen mouse gridSize) fld,
                    isOver = isMine (mapScreen mouse gridSize) renewedField,
                    mines = (Right m),
                    mode = mode
                } where
                    gridSize = playgroundSize mode 
                    renewedField = event (mapScreen mouse gridSize) fld
                    event :: Cell -> Field -> Field
                    event (c1, c2) f
                        | Map.member (c1, c2) f       = f           -- do not process a cell more than once
                        | Set.member (c1, c2) m       = openAllMines -- lost, open all mines
                        | otherwise = if isMineNeighbour
                                        then openCellSafe   -- just open a cell
                                        else openAllSafe    -- go through all neighbours
                        where
                            -- explore neighbours
                            neighbours :: [Cell]
                            neighbours = Prelude.filter checkBounds
                                $ Prelude.map (\(a, b) -> (a + c1, b + c2)) moves
                                    where
                                        checkBounds = \(a, b) -> (0 <= a && a < x) && (0 <= b && b < y) -- ensure we dont leave the grid
                                        moves = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)] -- steps to explore cells around

                            countNeighbours :: Int
                            countNeighbours = length $ Prelude.filter (`Set.member` m) neighbours

                            isMineNeighbour :: Bool
                            isMineNeighbour = not $ (0 ==) countNeighbours

                            openCellSafe :: Field
                            openCellSafe = addCell (c1, c2) (Checked countNeighbours) f

                            openAllSafe :: Field
                            openAllSafe = Prelude.foldr event openCellSafe neighbours
                            
                            openAllMines :: Field
                            openAllMines = Prelude.foldr event openCellMine $ Set.elems m
                                where openCellMine = addCell (c1, c2) Mine f

                            x = fst $ playgroundSize mode
                            y = snd $ playgroundSize mode  
        gs -> gs
-- right mouse click action
actionHandler (EventKey (MouseButton RightButton) Down m mouse) GameState 
    { 
        field = fld, 
        mines = (Right mines), 
        isOver = False,
        mode = mode 
    }
     = case Map.lookup (mapScreen mouse gridSize) fld of
            Nothing -> GameState -- add flag 
                {
                    field = Map.insert (mapScreen mouse gridSize) Flag fld,
                    mines = (Right mines),
                    isOver = False, 
                    mode = mode 
                }
            Just Flag -> GameState -- remove flag
                { 
                    field = Map.delete (mapScreen mouse gridSize) fld,
                    mines = (Right mines), 
                    isOver = False, 
                    mode = mode 
                }
            where
                gridSize = playgroundSize mode
-- restart game
actionHandler (EventKey (Char 'r') Down _ _) GameState { isOver = True, mode = mode }
    -- use newStdGen instead of simply getStdgen - to the current global random generator, updates it with one of the results, and returns the other.
     = let gen = unsafePerformIO newStdGen in getInitialModel gen mode

actionHandler _ gs = gs -- do not support other actions
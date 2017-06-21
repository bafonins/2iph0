import Data.Map as Map
import Data.Set as Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Random.Shuffle (shuffle')


-- game entry point
main :: IO ()
main = 
    putStrLn "the number of cells NxN, N = ?" >>
    getLine >>= \ n ->
    putStrLn "#mines = ?" >>
    getLine >>= \ m ->
    putStrLn ("cells = " ++ n ++ ", mines = " ++ m) >>
    getStdGen >>= \ g -> game g

---------------------------------
--          Constants          --
---------------------------------
    
-- define the playground as a 15x15 grid
fieldGridSize@(fieldW, fieldH) = (15, 15) :: (Int, Int)
-- the number of mines
minesNr = 40 :: Int

---------------------------------
--       Helper functions      --
---------------------------------

-- simply shuffle cells of the grid
shuffle g l = shuffle' l (fieldW * fieldH - 1) g

-- applies function to each element of the pair
applyBoth :: (a -> b) -> (a, a) -> (b, b)
applyBoth g (k, c) = (g k, g c)

-- TODO: doc
createMines :: RandomGen g => g -> Cell -> Mines
createMines rndGen cell = Set.fromList $ take minesNr $ shuffle rndGen $
    [(i, j) | i <- [0 .. fieldW - 1] , j <- [0 .. fieldH - 1], not $ (i, j) == cell]

-- checks if the cells within the field is a mine
isMine :: Cell -> Field -> Bool
isMine c f = case Map.lookup c f of
    (Just Mine)   -> True
    _           -> False


addCell :: Cell -> CellState -> Field -> Field
addCell cell state field = Map.insert cell state field

---------------------------------
--           Types             --
---------------------------------

createField :: Field
createField = Map.fromList []

type Field = Map Cell CellState
type Cell = (Int, Int)
type Mines = Set Cell

-- saves state of the cell, it can be :
--      explored (after the user clicked on it), 
--            we also display the number of mines around this cell (Checked)
--      there is a mine hidden (Mine)
--      user flagged it (Flag)
data CellState = Checked Int | Mine | Flag

-- keeps the state of the game
data GameState = GS
    { 
        field   :: Field, 
        mines   :: Either StdGen Mines,
        isOver  :: Bool
    }

---------------------------------
-- Define behaviour of the GUI --
---------------------------------

-- iniial state
ini gen = GS createField (Left gen) False

game :: StdGen -> IO ()
-- 'play' is for Gloss to manage custom mouse press events
game gen = play (InWindow "2IPH0" windowSize (500, 500)) white 50 (ini gen) renderer eventHandler id

-- map cells to the screen and place them proerly
windowSize = applyBoth (* (round cellSize)) fieldGridSize
cellSize = 24 :: Float
cellToScreen = applyBoth ((* cellSize) . fromIntegral)
viewPort = ViewPort (applyBoth (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldGridSize) 0 1

-- events
eventHandler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { 
        mines = Left gen
    } = gs 
        { mines = Right $ createMines gen cell }
            where
                cell = screenToCell mouse

eventHandler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { 
        field = field, 
        mines = Right mines,
        isOver = False
    } = gs
    { 
        field = renewedField,
        isOver = mineExploded
    } where
        mineExploded = isMine cell renewedField
        renewedField = click cell field
        cell@(cx, cy) = screenToCell mouse
        click :: Cell -> Field -> Field
        click c@(cx, cy) f
            | Map.member c f       = f                  -- do not process a cell more than once
            | Set.member c mines   = addCell c Mine f   -- mine
            | otherwise = let nf = addCell c (Checked neighbours) f in
                if neighbours == 0
                    then Prelude.foldr click nf neighbourCells -- Go through all neighbours
                    else nf
            where
                neighbourCells = [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1]
                                , 0 <= i && i < fieldW
                                , 0 <= j && j < fieldH
                                ] -- ;( 0 <= i < fieldW
                neighbours = length $ Prelude.filter (`Set.member` mines) neighbourCells

eventHandler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS
    { 
        field = field
    } = case Map.lookup coord field of
            Nothing -> gs { field = Map.insert coord Flag field }
            Just Flag -> gs { field = Map.delete coord field }
            _ -> gs
            where 
                coord = screenToCell mouse
eventHandler _ gs = gs
screenToCell = applyBoth (round . (/ cellSize)) . invertViewPort viewPort

renderer GS { field = field } = applyViewPortToPicture viewPort $ pictures $ cells ++ grid where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. fieldW - 1], y <- [0 .. fieldH - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ drawCell x y | x <- [0 .. fieldW - 1], y <- [0 .. fieldH - 1]]
    drawCell x y = case Map.lookup (x, y) field of
        Nothing         -> color white $ rectangleSolid cellSize cellSize -- the empty cell
        Just Mine       -> pictures [ color red $ rectangleSolid cellSize cellSize
                                    , label "M"
                                    ]
        Just (Checked n) -> pictures [ color green $ rectangleSolid cellSize cellSize
                                    , label $ show n
                                    ]
        Just Flag       -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    , label "F"
                                    ]
    label = translate (-5) (-5) . scale 0.14 0.14 . color black . text -- place labels properly
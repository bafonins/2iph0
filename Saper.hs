import Data.Map as Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Random.Shuffle (shuffle')

main :: IO ()
main = do
    gen <- getStdGen
    startGame gen

fieldSize@(fieldWidth, fieldHeight) = (15, 15) :: (Int, Int)
mineCount = 40 :: Int

createField :: Field
createField = Map.empty

type Field = Map Cell CellState --не забудем дописать вначале import import Data.Map
type Cell = (Int, Int)

data CellState = Opened Int --Открыта; параметр — циферка, которая будет отображаться
               | Mine       --Подорвались; без параметров
               | Flag       --Поставлен флажок

type Mines = Set Cell

createMines :: RandomGen g => g -> Cell -> Mines
createMines g fst = Data.Set.fromList $ take mineCount $ shuffle g $
    [(i, j) | i <- [0 .. fieldWidth - 1]
            , j <- [0 .. fieldHeight - 1]
            , (i, j) /= fst]

shuffle g l = shuffle' l (fieldWidth * fieldHeight - 1) g

data GameState = GS
    { field    :: Field
    , mines    :: Either StdGen Mines
    , gameOver :: Bool
    }

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "2IPH0" windowSize (240, 160)) white 30 (initState gen) renderer handler updater

windowSize = both (* (round cellSize)) fieldSize
cellSize = 24 :: Float

initState gen = GS createField (Left gen) False

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) --вспомогательная функция, которая ещё пригодится 

updater _ = id

cellToScreen = both ((* cellSize) . fromIntegral)

handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { mines = Left gen
    } = gs { mines = Right $ createMines gen cell } where
    cell = screenToCell mouse

handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field
    , mines = Right mines
    , gameOver = False
    } = gs
    { field = newField
    , gameOver = exploded
    } where
    newField = click cell field
    exploded = case Map.lookup cell newField of --Проигрыш, если последняя вскрытая клетка - мина
        Just Mine -> True
        _         -> False
    cell@(cx, cy) = screenToCell mouse
    click :: Cell -> Field -> Field
    click c@(cx, cy) f
        | c `Map.member` f     = f --повторно клетку не обрабатываем
        | c `Data.Set.member` mines = put Mine --попались на мину
        | otherwise = let nf = put (Opened neighbours) in
            if neighbours == 0
                then Prelude.foldr click nf neighbourCells --Обойдём соседей
                else nf
        where
            put state = Map.insert c state f
            neighbourCells = [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1]
                             , 0 <= i && i < fieldWidth
                             , 0 <= j && j < fieldHeight
                             ] --Жаль, нельзя написать 0 <= i < fieldWidth
            neighbours = length $ Prelude.filter (`Data.Set.member` mines) neighbourCells


handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS
    { field = field
    } = case Map.lookup coord field of
        Nothing -> gs { field = Map.insert coord Flag field }
        Just Flag -> gs { field = Map.delete coord field }
        _ -> gs
        where coord = screenToCell mouse
handler _ gs = gs
screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort

renderer GS { field = field } = applyViewPortToPicture viewPort $ pictures $ cells ++ grid where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ drawCell x y | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    drawCell x y = case Map.lookup (x, y) field of
        Nothing         -> color white $ rectangleSolid cellSize cellSize --клетка пустая
        Just Mine       -> pictures [ color red $ rectangleSolid cellSize cellSize
                                    , label "M"
                                    ]
        Just (Opened n) -> pictures [ color green $ rectangleSolid cellSize cellSize
                                    , label $ show n
                                    ]
        Just Flag       -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    , label "F"
                                    ]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text

viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1
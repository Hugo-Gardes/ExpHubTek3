import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data Direction = U | D | L | R

data Game = Game
  { snake :: [(Int, Int)],
    direction :: Direction,
    food :: (Int, Int),
    rng :: StdGen
  }

main :: IO ()
main = do
  rng_ <- newStdGen
  let initialGame =
        Game
          { snake = [(0, 0), (0, 1)],
            direction = R,
            food = (10, 10),
            rng = rng_
          }
  play (InWindow "Snake" (800, 600) (0, 0)) white 30 initialGame draw handleEvents step

draw :: Game -> Picture
draw game
  | null (snake game) = color green $ rectangleSolid 15 15
  | otherwise = pictures (snakeBody ++ [foodPicture])
  where
    snakeBody = map drawSquare (snake game)
    -- foodPicture = drawSquare (food game)
    drawSquare (x, y) = translate (fromIntegral x) (fromIntegral y) $ color green $ rectangleSolid 15 15
    foodSize = div (length (snake game)) 2
    foodSize' = fromIntegral foodSize :: Float
    foodSquare = translate (fromIntegral (fst (food game))) (fromIntegral (snd (food game))) $ color red $ rectangleSolid (15 * foodSize') (15 * foodSize')
    foodPicture = if foodSize' > 0 then foodSquare else blank

handleEvents :: Event -> Game -> Game
handleEvents (EventKey (Char 'z') _ _ _) game = game {direction = U}
handleEvents (EventKey (Char 'q') _ _ _) game = game {direction = L}
handleEvents (EventKey (Char 's') _ _ _) game = game {direction = D}
handleEvents (EventKey (Char 'd') _ _ _) game = game {direction = R}
handleEvents _ game = game

step :: Float -> Game -> Game
step _ game =
  if isGameOver newSnake
    then game
    else
      Game
        { snake = newSnake,
          direction = direction game,
          food = if head newSnake == food game then generateFood (rng game) newSnake else food game,
          rng = rng_
        }
  where
    newSnake = moveSnake (snake game) (direction game)
    rng_ = if head newSnake == food game then snd (random (rng game) :: (Int, StdGen)) else rng game

moveSnake :: [(Int, Int)] -> Direction -> [(Int, Int)]
moveSnake snake_ direction_ =
  let (x, y) = head snake_
      newHead = case direction_ of
        U -> (x, y + 1)
        D -> (x, y - 1)
        L -> (x - 1, y)
        R -> (x + 1, y)
   in newHead : init snake_

generateFood :: StdGen -> [(Int, Int)] -> (Int, Int)
generateFood rng_ snake_ =
  let maxX = 800
      maxY = 600
      freeCells = filter (`notElem` snake_) [(x, y) | x <- [0 .. maxX], y <- [0 .. maxY]]
      (randomIndex, _) = randomR (0, length freeCells - 1) rng_
   in freeCells !! randomIndex

isGameOver :: [(Int, Int)] -> Bool
isGameOver snake_ =
  let (x, y) = head snake_
   in x < -400 || x > 400 || y < -300 || y > 300 || length snake_ /= length (nub snake_)

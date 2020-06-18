type Loc = (Integer, Integer)
type Map = [Loc]

width :: Integer
width = 5

height :: Integer
height = 5

example :: Map
example = [(1,2), (2,2), (3,2)]

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : (removeDups $ filter (/= x) xs)

wrap :: Loc -> Loc
wrap (x,y) = (mod x width, mod y height)

neighbors :: Loc -> [Loc]
neighbors (x,y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

countNeighbors :: Loc -> Map -> Int
countNeighbors l m = length $ filter (flip elem m) $ neighbors l

survivors :: Map -> [Loc]
survivors m = [l | l <- m, elem (countNeighbors l m) [2,3]]

births :: Map -> [Loc]
births m = [l | l <- removeDups (concat (map neighbors m)), not $ elem l m, countNeighbors l m == 3]

nextgen :: Map -> Map
nextgen m = survivors m ++ births m

pp :: Map -> IO [[()]]
pp m = sequence [ppl x | x <- [0..height-1]]
  where
    ppl x = sequence [putStr $ getStr x y | y <- [0..width]]
    getStr x y | y == width = "\n"
               | elem (x,y) m = "#"
               | otherwise = "."

life :: Map -> IO ()
life m = do
          pp m
          cmd <- getLine
          if cmd == "q" then return () else life (nextgen m)

main :: IO ()
main = life example

type Loc = (Integer, Integer)
type Map = [Loc]

example :: Map
example = [(0,0), (0,1), (0,2), (1,0), (1,2), (2,0), (2,1), (2,2), (3,0), (3,1), (3,2), (4,0), (4,1), (4,2), (5,0), (5,1), (5,2), (6,0), (6,2), (7,0), (7,1), (7,2)]

removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : (removeDups $ filter (/= x) xs)

neighbors :: Loc -> [Loc]
neighbors (x,y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

countNeighbors :: Loc -> Map -> Int
countNeighbors l m = length $ filter (flip elem m) $ neighbors l

survivors :: Map -> [Loc]
survivors m = [l | l <- m, elem (countNeighbors l m) [2,3]]

births :: Map -> [Loc]
births m = [l | l <- removeDups (concat (map neighbors m)), not $ elem l m, countNeighbors l m == 3]

nextgen :: Map -> Map
nextgen m = survivors m ++ births m

prettyPrint :: Map -> IO [()]
prettyPrint m = prettyPrint' m (minimum xs - 1) (maximum xs + 1) (minimum ys - 1) (maximum ys + 1)
  where
    xs = map fst m
    ys = map snd m
    prettyPrint' m minX maxX minY maxY = sequence [printLn x | x <- [minX..maxX]]
      where
        printLn x = putStrLn [getCh x y | y <- [minY..maxY]]
        getCh x y | elem (x,y) m = '#'
                  | otherwise = '.'

life :: Map -> IO ()
life m = do
          prettyPrint m
          cmd <- getLine
          if cmd == "q" then return () else life (nextgen m)

main :: IO ()
main = life example

import qualified Data.Set as S
import qualified Data.Map as M
import System.Environment
import System.CPUTime
import Random
import Control.Applicative ((<$>))

-- http://www.morpionsolitaire.com/




-- Data --
--------------------------------------------------------------------------------

type Cross = (Int, Int)

data Edge  = Edge Cross Int Int
           deriving (Show, Ord, Eq)

data Board = Board (S.Set Cross) (S.Set Edge)

data Move  = Move Cross [Edge]
           deriving (Show)

defaultBoard :: String
defaultBoard =  ". . . O O O O . . .\n" ++
                "                   \n" ++
                ". . . O . . O . . .\n" ++
                "                   \n" ++
                ". . . O . . O . . .\n" ++
                "                   \n" ++
                "O O O O . . O O O O\n" ++
                "                   \n" ++
                "O . . . . . . . . O\n" ++
                "                   \n" ++
                "O . . . . . . . . O\n" ++
                "                   \n" ++
                "O O O O . . O O O O\n" ++
                "                   \n" ++
                ". . . O . . O . . .\n" ++
                "                   \n" ++
                ". . . O . . O . . .\n" ++
                "                   \n" ++
                ". . . O O O O . . .\n"





-- Util --
--------------------------------------------------------------------------------

interleave :: [a] -> [a]    -> [a]
interleave    []     []     =  []
interleave    []     (x:xs) =  x : interleave [] xs
interleave    (x:xs) []     =  x : interleave xs []
interleave    (x:xs) (y:ys) =  x : y : interleave xs ys 

boundaries :: S.Set Cross -> (Cross, Cross)
boundaries    crosses       =  ((bound min fst, bound min snd), (bound max fst, bound max snd))
  
    where bound pred orient = let xs = map orient $ S.elems crosses
                              in  foldr pred (head xs) (tail xs)
                                




-- Rendering & Parsing --
--------------------------------------------------------------------------------

instance Show Board where
  
  show (Board crosses edges) = concat $ interleave drawOddRows drawEvenRows
                               
    where ((minRow, minCol), (maxRow, maxCol)) = boundaries crosses 

          drawCrosses x         = do y <- [minCol .. maxCol]
                                     if (x, y) `S.member` crosses then "O" else "."        
          
          drawBetweenCrosses x  = do y <- [minCol .. maxCol - 1]
                                     if Edge (x, y) 0 1 `S.member` edges then "-" else " "

          drawOddColumns x      = do y <- [minCol .. maxCol]
                                     if Edge (x, y) 1 0 `S.member` edges then "|" else " "

          drawOddRows           = do x <- [minRow .. maxRow] 
                                     return $ interleave (drawCrosses x) (drawBetweenCrosses x) ++ "\n"

          drawEvenRows          = [ drawEvenRowEdges x ++ "\n" | x <- [minRow .. maxRow - 1] ] 
          drawEvenColumns x     = map (drawCell x) [minCol .. maxCol - 1]
          drawEvenRowEdges x    = interleave (drawOddColumns x) (drawEvenColumns x)
          drawCell  x y         = case (Edge (x, y) 1 1 `S.member` edges, Edge (x + 1, y) (-1) 1 `S.member` edges) of
                                    (True,  True)  -> 'X'
                                    (True,  False) -> '\\'
                                    (False, True)  -> '/'
                                    (False, False) -> ' '
          
          

  
  
parse :: String -> Board
parse    string =  Board (S.fromList $ getCrosses 0 0 string) (S.fromList $ getEdges 0 0 string)

  where getCrosses x y [] = []
        getCrosses x y ('O':as)  = (x, y) : getCrosses x (y + 1) as
        getCrosses x y ('\n':as) = getCrosses (x + 1) 0 (drop 1 $ dropWhile (/= '\n') as) 
        getCrosses x y ('.':as)  = getCrosses x (y + 1) as
        getCrosses x y (_:as)    = getCrosses x y as 
        
        getEdges x y []          = []
        getEdges x y ('O':as)    = getEdges x (y + 1) as
        getEdges x y ('-':as)    = Edge (x, y - 1) 0 1 : getEdges x y as
        getEdges x y ('\n':as)   = getEdges' x 0 as
        getEdges x y ('.':as)    = getEdges x (y + 1) as
        getEdges x y (_:as)      = getEdges x y as
        
        getEdges' x y []         = []
        getEdges' x y ('\n':as)  = getEdges (x + 1) 0 as
        getEdges' x y ('|':as)   = Edge (x, y) 1 0 : getEdges'' x y as 
        getEdges' x y (_:as)     = getEdges'' x y as
        
        getEdges'' x y []        = []
        getEdges'' x y ('\n':as) = getEdges (x + 1) 0 as
        getEdges'' x y ('\\':as) = Edge (x, y) 1 1 : getEdges' x (y + 1) as
        getEdges'' x y ('/':as)  = Edge (x + 1, y) (-1) 1 : getEdges' x (y + 1) as
        getEdges'' x y ('X':as)  = Edge (x, y) 1 1 : Edge (x + 1, y) (-1) 1 : getEdges' x (y + 1) as
        getEdges'' x y (_:as)    = getEdges' x (y + 1) as





-- Computation --
--------------------------------------------------------------------------------

getMoves :: Board                 -> [Move]
getMoves    (Board crosses edges) =  [ move | x <- [minRow - 1 .. maxRow + 1], 
                                              y <- [minCol - 1 .. maxCol + 1], 
                                              move <- moves x y ]  
    
  where ((minRow, minCol), (maxRow, maxCol)) = boundaries crosses 

        moves x y | (x, y) `S.member` crosses = []
                  | otherwise                 = [ move | x' <- [-1 .. 1], 
                                                         y' <- [0 .. 1], 
                                                         z  <- [0  .. 4], 
                                                         x' /= 0 || y' /= 0, 
                                                         move <- isMove z 5 (x - (x' * z)) (y - (y' * z)) x' y' ]
          
            where isMove z n a b x' y' | n == 0 = let x0 = a - (5 * x')
                                                      y0 = b - (5 * y')
                                                      path n a b | n == 0    = []
                                                                 | otherwise = Edge (a, b) x' y' : path (n - 1) (a + x') (b + y') 
                                                  in  return $ Move (x, y) $ path 4 x0 y0

                                       | Edge (a, b) x' y' `S.member` edges  = []
                                       | z == 0 || (a, b) `S.member` crosses = isMove (z - 1) (n - 1) (a + x') (b + y') x' y'
                                       | otherwise                         = []
          
applyMove :: Board              -> Move                -> Board
applyMove    (Board crosses edges) (Move cross edges') =  Board (S.insert cross crosses) (S.union edges (S.fromList (edges' ++ (map dual edges'))))
        
  where dual (Edge (x, y) x' y') = Edge (x + x', y + y') (-x') (-y')

doMoves :: Int -> Board -> Board
doMoves    0      board =  board
doMoves    n      board =  doMoves (n - 1) (applyMove board (last $ getMoves board))





-- Strategy --
--------------------------------------------------------------------------------

type Strategy = StdGen -> [Move] -> Board -> (Move, StdGen)





-- | No Strategy - moves are chosen entirely at random from entropy source, with
--   each possible move equally likely

noStrategy :: Strategy
noStrategy    gen moves _ = let (i, gen') = randomR (0, length moves - 1) gen
                            in  (moves !! i, gen')
                                

-- | No Strategy - moves are chosen entirely at random from entropy source, with
--   each possible move equally likely

aStrategy :: Strategy
aStrategy    gen moves _ = (head moves, gen)





-- | Outside Strategy - Moves are split into the group of moves at the edge of 
--   the grid and those which aren't.  If there are moves in teh first group, a 
--   random moves is chosen from there - otherwise, a random move is chosen from
--   the entire move set.

outsideStrategy :: Strategy
outsideStrategy    gen moves board@(Board crosses _) | length priorityMoves > 0 = noStrategy gen priorityMoves board
                                                     | otherwise                = noStrategy gen moves board 

    where priorityMoves            = filter isOutside moves
          isOutside (Move (x,y) _) = x `elem` [minRow-1, maxRow+1] || y `elem` [minCol-1 , maxCol+1] 
         
          ((minRow, minCol), (maxRow, maxCol)) = boundaries crosses 





-- IO --
--------------------------------------------------------------------------------

main :: IO ()
main =  gameLoop 0 0

time :: IO t -> IO (Double, t)
time a = do start <- getCPUTime
            v     <- a
            end   <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            return (diff :: Double, v)




gameLoop :: Int -> Int -> IO ()
gameLoop    hi     tries=  do (t, (n, history)) <- time play
                              case n > hi of
                                False -> gameLoop hi (tries + 1)
                                True  -> do mapM_ (putStrLn . (++ "\n\n") . show) (reverse history)
                                            putStrLn ""
                                            putStrLn $ (show n) ++ " moves - " ++ (show tries) ++ " attempts"
                                            putStrLn $ (show t) ++ "s elapsed"
                                            putStrLn "\n==================================================================\n"
                                            gameLoop n (tries + 1)

play :: IO (Int, [Board])
play = do gen <- newStdGen
          play' gen 0 [parse defaultBoard]

       where play' gen n history@(board:_) = let moves = getMoves board
                                             in  case length moves == 0 of
                                               True -> return (n, history)
                                               False -> let (move, gen') = noStrategy gen moves board
                                                        in  play' gen' (n + 1) ((applyMove board move):history)

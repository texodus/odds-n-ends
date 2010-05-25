import qualified Data.Set as S
import System.Environment
import Random
import Control.Applicative ((<$>))

-- http://www.morpionsolitaire.com/




-- Data --
--------------------------------------------------------------------------------

type Cross = (Int, Int)
type Edge  = (Cross, Cross)

data Board = Board (S.Set Cross) (S.Set Edge)

data Move = Move Cross [Edge]

type Orientation = (Int, Int) -> Int

defaultBoard = ". . . # # # # . . .\n" ++
               "                   \n" ++
               ". . . # . . # . . .\n" ++
               "                   \n" ++
               ". . . # . . # . . .\n" ++
               "                   \n" ++
               "# # # # . . # # # #\n" ++
               "                   \n" ++
               "# . . . . . . . . #\n" ++
               "                   \n" ++
               "# . . . . . . . . #\n" ++
               "                   \n" ++
               "# # # # . . # # # #\n" ++
               "                   \n" ++
               ". . . # . . # . . .\n" ++
               "                   \n" ++
               ". . . # . . # . . .\n" ++
               "                   \n" ++
               ". . . # # # # . . .\n"





-- Util --
--------------------------------------------------------------------------------

interleave :: [a] -> [a]    -> [a]
interleave    []     []     =  []
interleave    []     (x:xs) =  x : interleave [] xs
interleave    (x:xs) []     =  x : interleave xs []
interleave    (x:xs) (y:ys) =  x : y : interleave xs ys 

row :: Orientation
row =  fst

column :: Orientation
column =  snd

boundaries :: S.Set Cross -> ((Int, Int), (Int, Int))
boundaries    crosses     =  ((bound min row, bound min column), (bound max row, bound max column))
  
    where bound pred orient = let xs = map orient $ S.elems crosses
                              in  foldr pred (head xs) (tail xs)
                                
instance Show Board where
  
  show (Board crosses edges) = concat $ interleave drawOdd drawEven
                               
    where drawOdd        = [interleave (drawCross x) (drawOddEdge x) ++ "\n" | x <- [minRow .. maxRow]]
          drawCross x    = [if S.member (x, y) crosses then 'O' else '.' | y <- [minCol .. maxCol]]       
          drawOddEdge x  = [if S.member ((x, y), (x, y + 1)) edges then '-' else ' ' | y <- [minCol .. maxCol - 1]]
          drawEven       = [drawEvenEdge x ++ "\n" | x <- [minRow .. maxRow - 1]] 
          drawEvenEdge x = interleave (drawCols x) (drawXs x)
          drawCols x     = [if S.member ((x, y), (x + 1, y)) edges then '|' else ' ' | y <- [minCol .. maxCol]]
          drawXs x       = [drawCell x y | y <- [minCol .. maxCol - 1]]
          drawCell x y   = case ((S.member ((x, y), (x + 1, y + 1)) edges), (S.member ((x + 1, y), (x, y + 1)) edges)) of
                             (True,  True)  -> 'X'
                             (True,  False) -> '\\'
                             (False, True)  -> '/'
                             (False, False) -> ' '
          
          ((minRow, minCol), (maxRow, maxCol)) = boundaries crosses
  
  
parse :: String -> Board
parse    string =  Board (S.fromList $ getCrosses 0 0 string) (S.fromList $ getEdges 0 0 string)

  where getCrosses x y [] = []
        getCrosses x y ('#':as)  = (x, y) : getCrosses x (y + 1) as
        getCrosses x y ('\n':as) = getCrosses (x + 1) 0 (drop 1 $ dropWhile (/= '\n') as) 
        getCrosses x y ('.':as)  = getCrosses x (y + 1) as
        getCrosses x y (_:as)    = getCrosses x y as
        
        getEdges x y []          = []
        getEdges x y ('#':as)    = getEdges x (y + 1) as
        getEdges x y ('-':as)    = ((x, y), (x, y - 1)) : ((x, y - 1), (x, y)) : getEdges x y as
        getEdges x y ('\n':as)   = getEdges' x 0 as
        getEdges x y ('.':as)    = getEdges x (y + 1) as
        getEdges x y (_:as)      = getEdges x y as
        
        getEdges' x y []         = []
        getEdges' x y ('\n':as)  = getEdges (x + 1) 0 as
        getEdges' x y ('|':as)   = ((x,y),(x+1,y)) : ((x+1,y),(x,y)) : getEdges'' x y as 
        getEdges' x y (_:as)     = getEdges'' x y as
        
        getEdges'' x y []        = []
        getEdges'' x y ('\n':as) = getEdges (x + 1) 0 as
        getEdges'' x y ('\\':as) = ((x,y),(x+1,y+1)) : ((x+1,y+1),(x,y)) : getEdges' x (y + 1) as
        getEdges'' x y ('/':as)  = ((x+1,y),(x,y+1)) : ((x,y+1),(x+1,y)) : getEdges' x (y + 1) as
        getEdges'' x y ('X':as)  = ((x,y),(x+1,y+1)) : ((x+1,y+1),(x,y)) : ((x+1,y),(x,y+1)) : ((x,y+1),(x+1,y)) : getEdges' x (y + 1) as
        getEdges'' x y (_:as)    = getEdges' x (y + 1) as





-- Computation --
--------------------------------------------------------------------------------

getMoves :: Board                 -> [Move]
getMoves    (Board crosses edges) =  [move | x <- [minRow - 1.. maxRow + 1], y <- [minCol - 1 .. maxCol + 1], move <- moves x y]  
    
  where ((minRow, minCol), (maxRow, maxCol)) = boundaries crosses 

        moves x y | (x,y) `S.member` crosses = []
                  | otherwise                = [ move | x' <- [-1 .. 1], 
                                                        y' <- [-1 .. 1], 
                                                        z  <- [0  .. 4], 
                                                        x' /= 0 || y' /= 0, 
                                                        move <- isMove z 5 (x - (x' * z)) (y - (y' * z)) x' y' x y ]
          
            where isMove q n x y x' y' a b | n == 0 = let (x0, y0) = (x - (5 * x'), y - (5 * y'))
                                                          path n x y | n == 0    = []
                                                                     | otherwise = ((x,y),(x+x',y+y')) : path (n - 1) (x + x') (y + y') 
                                                      in  return $ Move (a, b) $ path 4 x0 y0

                                           | ((x,y), (x+x',y+y')) `S.member` edges = []
                                           | q == 0 || (x,y) `S.member` crosses    = isMove (q - 1) (n - 1) (x + x') (y + y') x' y' a b
                                           | otherwise                             = []
            
        
          
applyMove :: Board              -> Move                -> Board
applyMove    (Board crosses edges) (Move cross (a:b:c:d:[])) =  Board (S.insert cross crosses) (S.union newEdges edges)

    where newEdges = S.union (S.fromList [a,b,c,d]) (S.fromList (map rev [a,b,c,d]))
          rev ((a,b),(c,d)) = ((c,d),(a,b))
        
doMoves :: Int -> Board -> Board
doMoves    0      board =  board
doMoves    n      board =  doMoves (n - 1) (applyMove board (last $ getMoves board))





-- Strategy --
--------------------------------------------------------------------------------

type Strategy = StdGen -> [Move] -> Board -> (Move, StdGen)

noStrategy :: Strategy
noStrategy    gen moves _ = let (i, gen') = randomR (0, length moves - 1) gen
                            in  (moves !! i, gen')

outsideStrategy :: Strategy
outsideStrategy    gen moves board@(Board crosses _) = case length priorityMoves > 0 of
                                                         True  -> noStrategy gen priorityMoves board
                                                         False -> noStrategy gen moves board 

    where priorityMoves            = filter isOutside moves
          isOutside (Move (x,y) _) = x `elem` [minRow-1, maxRow+1] || y `elem` [minCol-1 , maxCol+1] 
         
          ((minRow, minCol), (maxRow, maxCol)) = boundaries crosses 





-- IO --
--------------------------------------------------------------------------------

main :: IO ()
main =  gameLoop 0 0

gameLoop :: Int -> Int -> IO ()
gameLoop    hi     tries=  do (n, board) <- play
                              case n > hi of
                                False -> gameLoop hi (tries + 1)
                                True  -> do putStrLn $ (show n) ++ " moves - " ++ (show tries) ++ " attempts"
                                            putStrLn ""
                                            putStrLn (show board)
                                            putStrLn ""
                                            gameLoop n (tries + 1)

play :: IO (Int, Board)
play = do gen <- newStdGen
          play' gen 0 $ parse defaultBoard

       where play' gen n board = let moves = getMoves board
                                 in  case length moves == 0 of
                                       True -> return (n, board)
                                       False -> let (move, gen') = outsideStrategy gen moves board
                                                in  play' gen' (n + 1) (applyMove board move)

                                             
                                           


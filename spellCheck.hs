


module SpellCheck (generateDictionary, spellCheck, Dictionary, Word) where

import qualified Data.Map as M
import Data.Char (toLower, toUpper)
import Data.List (foldl', union, intersect)
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
import Test.QuickCheck
import Control.Monad



-- main = do putStrLn "loading ..."
--           (path:_)   <- getArgs
--           file       <- openFile path ReadMode
--           contents   <- hGetContents file
--           loop $ generateDictionary contents

--     where loop dict = dict `seq` do putStr ">" >> hFlush stdout
--                                     words <- liftM (split . (map toLower)) getLine
--                                     putStrLn $ "Did you mean '" ++  (clean dict words) ++ "'?"
--                                     loop dict
         
--           clean dict xs = intercalate " " $ spellCheck dict xs
          
--           split []   = []
--           split text = (takeWhile (\x -> x /= ' ') text) : (split (drop 1 (dropWhile (\x -> x /= ' ') text)))


---- Interface

newtype Dictionary = D (M.Map String Int)
type    Word       = String

generateDictionary :: String -> Dictionary
generateDictionary input = D $ generateDictionary' input

spellCheck :: Dictionary -> [Word] -> [Word]
spellCheck (D dict) words = map (correct dict) words

---- Correction

alphabet = "abcdefghijklmnopqrstuvwxyz"

generateDictionary' :: String -> M.Map String Int
generateDictionary'    input  =  case parse wordParser "dictionary" (map toLower input) of
                                   (Right d) -> train d
                                   _         -> M.empty

    where wordParser = do spaces 
                          c <- many1 (lower <|> upper) `endBy` many (noneOf alphabet)
                          eof
                          return c 

train :: [String] -> M.Map String Int
train    words    =  foldl' assoc (M.empty) words

    where assoc map word = M.insertWith' (+) word 1 map

known :: M.Map String Int -> [String] -> [String]
known    dictionary          words    =  filter (flip M.member dictionary) words

edits :: Int -> String -> [String]
edits    n      word 
    | n == 1           =  foldl' union [] [deletes, transposes, replaces, inserts]
    | otherwise        =  edits (n - 1) word >>= (edits 1) -- [y | x <- edits (n - 1) word, y <- (edits 1 x)]

    where deletes      =  [(sub 0 x) ++ (sub (x + 1) len) | x <- [0..(len - 1)]]
          transposes   =  [(sub 0 x) ++ [(word !! (x + 1)), (word !! x)] ++ (sub (x + 2) len) | x <- [0..(len - 2)]]
          replaces     =  [(sub 0 x) ++ [c] ++ (sub (x + 1) len) | x <- [0..(len - 1)], c <- alphabet]
          inserts      =  [(sub 0 x) ++ [c] ++ (sub x len) | x <- [0..len], c <- alphabet]
     
          sub i j      =  take (j - i) $ drop i $ word
          len          =  length word

correct :: M.Map String Int -> String -> String
correct    dictionary          word   =  snd $ foldr match (1, word) $ candidates
  
    where match new (c,old) = case M.lookup new dictionary of
                                (Just d) | d > c -> (d, new)
                                otherwise        -> (c, old)

          candidates   = head $ filter ((>0) . length) permutations
          permutations = (++ [[word]]) $ map (known dictionary) [[word], (edits 1 word), (edits 2 word)]


---- Tests

data TestSet = TS (M.Map String Int) [String] [String] deriving (Show)

instance Arbitrary TestSet where
    coarbitrary = undefined
    arbitrary   = do common  <- (filter (/="")) <$> resize 5 arbitrary
                     known   <- (filter (/="")) <$> resize 5 arbitrary
                     unknown <- (filter (/="")) <$> resize 5 arbitrary
                     counts  <- ((map ((+1) . abs)) . (cycle . (++[1]))) <$> resize 5 arbitrary
                     return $ TS (M.fromList (zipWith (,) (known ++ common) counts)) (unknown ++ common) common

instance Arbitrary Char where
    coarbitrary = undefined 
    arbitrary   = oneof (map return (alphabet ++ (map toUpper alphabet)))

prop_cross (TS d w c) = collect (length c) $ intersect c (intersect w (spellCheck (D d) w)) == c

prop_idemp (TS d w _) = spellCheck (D d) w == spellCheck (D d) (spellCheck (D d) w)

prop_edits w          = length w > 1 ==> 54 * (length w) + 25 >= (length $ edits 1 w)

prop_parse (TS _ w _) = let x = M.size $ generateDictionary' $ w >>= (++" ")
                        in length w > 0 ==> x > 0 && x <= length w 










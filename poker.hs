{-# LANGUAGE ExistentialQuantification #-}





-- Five Card Haskell --
--------------------------------------------------------------------------------

import GHC.Base 
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>))
import Data.List (tails, permutations, sort, group, sortBy)
import Data.Char (toUpper)
import Random
import System.Environment
import System.IO
import System.IO.Unsafe






-- Data Types --
--------------------------------------------------------------------------------

-- | Represents a Suit.  Ord is designed such that all Suits are equivalent -
--   this determines some aspects of scoring behavior since Ord Card derives 
--   from it

data Suit = Spades | Hearts | Diamonds | Clubs
          deriving (Show, Eq, Enum)

instance Ord Suit where
  compare _ _ = EQ
    




-- | Represents a Rank.  

data Rank = Two | Three | Four | Five | Six | Seven | Eight 
          | Nine | Ten | Jack | Queen | King | Ace
          deriving (Show, Ord, Eq, Enum)





-- | Represents a Card.  The instance of Random allows use to generate unique
--   random cards, as long as the generator supplied is a Deck

data Card = Card {rank :: Rank, suit :: Suit}            
          deriving (Eq, Ord)
                   
instance Enum Card where 
  
  fromEnum (Card rank suit) = (fromEnum suit) * 13 + (fromEnum rank)
  toEnum   enum             = Card (toEnum $ enum `mod` 13) (toEnum $ enum `quot` 13) 
 
instance Random Card where
  
  randomR  = undefined
  random g = let (index, g')  = next g
             in  (toEnum index, g')

instance Show Card where
  
  show (Card v s) = (show v) ++ " of " ++ (show s)
  

  




-- | Represents a Deck.  Instance of RandomGen requires the return type of 
--   next to be Int, so Deck cannot be an actually [Card], but isntead must 
--   function as a mapping between Card and Int through the Enum class.

newtype Deck = Deck [Int] 
             deriving (Show)

instance RandomGen Deck where
  
  next  (Deck (x:xs)) = (x, (Deck xs))
  next  (Deck [])     = undefined
  genRange g          = (0, 51)
  split (Deck xs)     = let pivot = length xs `quot` 2
                        in  (Deck $ take pivot xs, Deck $ drop pivot xs)





-- | Represents the score of a poker hand.  Each value encapsulates only enough
--   data to rank a Score against another Score by official rules.

data Score = HighCard      Rank      [Rank]
           | Pair          Rank      [Rank]
           | TwoPair       Rank Rank [Rank]
           | ThreeOfAKind  Rank      [Rank] 
           | FourOfAKind   Rank      [Rank]
           | FullHouse     Rank Rank
           | Flush         Suit      [Rank]
           | Straight      Rank
           | StraightFlush Rank Suit 
           deriving (Eq, Ord)
                    
instance Show Score where
   show (HighCard r _)      = "High Card " ++ show r
   show (Pair r _)          = "Pair of " ++ show r ++ "s"
   show (TwoPair r r' _)    = show (Pair r []) ++ ", " ++ show (Pair r' [])
   show (ThreeOfAKind r _)  = "Three " ++ show r ++ "s"
   show (FourOfAKind r _)   = "Four " ++ show r ++ "s"
   show (FullHouse r r')    = "Full House, " ++ show r ++ "s and " ++ show r' ++ "s"
   show (Flush s _)         = "Flush (" ++ show s ++ ")"
   show (Straight r)        = "Straight, starting with " ++ show r
   show (StraightFlush r s) = "Straight Flush (" ++ show s ++ "), starting with " ++ show r




-- | Represents statistics about a game state.  Win is the probability of winning 
--   a hand, Loss is the probability of losing a hand;  Tie is the probability of
--   any other player having the same score (so a tie is not exclusive to a win 
--   or loss)

data Odds = forall a. (Fractional a) => Odds a a Score

instance Show Odds where
  
  show (Odds x y s) = let format = (++ "%") . (take 5) . show . (* 100)
                      in  "\n"   
                          ++ "  Win:  " ++ (format x) ++ "\n"
                          ++ "  Tie:  " ++ (format y) ++ "\n"
                          ++ "  Loss: " ++ (format (1 - (x + y))) ++ "\n"
                          ++ "\n"
                          ++ "  Most likely outcome: " ++ (show s) ++ "\n"
                    
                    
                    
                    
                    
-- IO --
--------------------------------------------------------------------------------

-- | Takes a list of player names as input - first name is assumed to be both
--   user and first dealer.

main :: IO ()
main =  do count <- length <$> getArgs
           case count < 2 of
             True  -> putStrLn "At least two players are required"
             False -> do (player:opponents) <- getArgs
                         mainLoop player opponents
           
  where mainLoop p ps = do putStrLn $ take 80 $ repeat '-'
                           winner <- gameLoop p ps
                           mainLoop p $ (drop 1 ps) ++ [head ps]
                            
        gameLoop p ps = do hand  <- getCards p 2
                           flop  <- getCards "flop"  3
                           putStrLn $ show $ odds 7 hand flop
                           turn  <- getCards "turn"  1
                           putStrLn $ show $ odds 7 hand $ flop ++ turn
                           river <- getCards "river" 1
                           putStrLn $ show $ odds 7 hand $ flop ++ turn ++ river
        
        getCards l x  = do putStr $ l ++ " : "
                           hFlush stdout
                           cards <- (++ "  ") <$> getLine
                           case parse cardParser "Cards" cards of 
                             (Right cards) | length cards == x -> return cards
                             _____                             -> do putStrLn "Error Parsing Cards"
                                                                     getCards l x
                             




-- | Parser for reading a list of space seperated cards into a list of Card
--   types.  Sample = 'AH KH QH JS 10C'                                                                     
                             
cardParser :: Parser [Card]
cardParser =  manyTill pCard eof

  where pCard = do r <- pRank
                   s <- pSuit
                   many (char ' ')
                   return $ Card r s
                 
        pSuit = do c <- oneOf "HDCShdcs"
                   return $ case toUpper c of
                              'H' -> Hearts
                              'D' -> Diamonds
                              'C' -> Clubs
                              'S' -> Spades
                     
        pRank = do c <- oneOf "123456789JQKAjqka"
                   case toUpper c of
                     'A' -> return Ace
                     'K' -> return King
                     'Q' -> return Queen
                     'J' -> return Jack
                     '1' -> do char '0'
                               return Ten
                     x   -> return $ toEnum $ read (x:[]) - 2
                   



                   
-- | Creates a new Deck, which represents a random ordering of n decks of 52 
--   unique playing cards.  This design is flawed in that behavior once 52
--   generates have been split is undefined - there is no way to represent
--   an empty deck when random is called with RandomGen Deck
                     
shuffle :: Int -> IO Deck 
shuffle    n  =  let ds = 51 * n 
                 in Deck <$> shuffle' [0..ds] ds
  
  where shuffle' _  0   = return []
        shuffle' xs len = do (y, ys) <- (choose xs) <$> randomRIO (0, len-1)
                             ys'     <- shuffle' ys (len - 1)
                             return (y:ys')
                             
        choose []     _ = undefined
        choose (x:xs) 0 = (x, xs)
        choose (x:xs) i = let (y, ys) = choose xs (i - 1)
                          in (y, x:ys)
  




-- SCORING
--------------------------------------------------------------------------------
        
-- | Determines the Score from a list of Cards by dispatching on a 4-tuple of 
--   properties that can uniquely determine a score via pattern matching (and a
--   few simple guards) - length of the longest run of Ranks, length of the 
--   second longest run of Ranks, isStraight and isFlush.
        
score :: [Card] -> Score
score    cards  =  case (length (head runs), length (head (drop 1 runs)), isStraight ranks, isFlush suits) of
                     (_, _, Just r, Just s)        -> StraightFlush r s
                     (x, _, _, _) | x > 3          -> FourOfAKind  a rest
                     (x, y, _, _) | x > 2 && y > 1 -> FullHouse a b
                     (_, _, Just r, ______)        -> Straight r
                     (_, _, ______, Just s)        -> Flush s rest
                     (3, _, _, _)                  -> ThreeOfAKind a rest  
                     (2, 2, _, _)                  -> TwoPair a b rest
                     (2, _, _, _)                  -> Pair a rest
                     ____________                  -> HighCard a rest
  
  where runs             = reverse $ sortBy (\x y -> compare (length x) (length y)) ranks 
        (a:b:rest)       = map head runs
        ranks            = group $ sort $ map rank cards
        suits            = group $ map (toEnum) $ sort $ map (fromEnum . suit) cards
        
        isFlush suits    = let flushes = filter ((> 4) . length) suits
                           in  case length flushes == 1 of
                                 True  -> Just $ head $ head flushes
                                 False -> Nothing

        isStraight ranks = let seqs      = filter (seq . (take 5)) orderings
                               orderings = filter ((>4) . length) $ tails $ map (fromEnum . head) ranks
                               seq xs    = ((foldr min 13 xs) == ((foldr max 0 xs) - 4)) 
                                           || (length [x | x <- xs, x `elem` [0,1,2,3,12]] == 5)
                           in  case length seqs > 0 of
                                 False -> Nothing
                                 True  -> Just $ toEnum $ head $ head $ reverse seqs
  




-- | Determines the list of all possible scores given a hand and n additional        
--   unknown cards to be drawn from the deck.                                 

scores :: [Card] -> Int -> [Score]
scores    hand      n   =  let deck  = [y | y <- map toEnum [0..51], y `notElem` hand]
                               draws = map (hand ++) $ nCombinations (n - length hand) deck 
                           in  map score draws





-- | Calculates the odds of winning a hand of n cards against ps opponents,
--   with game state hand and flop.  Calculates all possible future game states
--   from thsi information and coutns wins

odds :: Int -> [Card] -> [Card] -> Odds
odds    n      hand      flop   =  Odds (win / total) (tie / total) hiscore
  
  where hiscore     = head $ head $ reverse $ sortBy (\x y -> compare (length x) (length y)) $ group $ sort $ scores (hand ++ flop) n
    
        win         = fromIntegral (length (filter (==GT) record))        
        tie         = fromIntegral (length (filter (==EQ) record))
        total       = fromIntegral (length record)
        
        record      = map isWin matches

        isWin pair  = let (flop', hand') = pair     
                      in  compare (score $ hand ++ flop ++ flop') (score $ hand' ++ flop ++ flop')
        
        matches     = [(flop', hand') | flop' <- nCombinations (n - (length flop + length hand)) deck,
                                        hand' <- nCombinations (length hand) deck,
                                        length [x | x <- hand', x `elem` flop'] == 0]
        
        deck        = [y | y <- map toEnum [0..51], y `notElem` (hand ++ flop)]
                                        




-- | Calculates all combinations of length n of a list.  Does not check for 
--   duplciates.

nCombinations :: Int -> [a] -> [[a]]  
nCombinations    0      ___ =  [[]]
nCombinations    n      xs  =  do y:xs' <- tails xs 
                                  ys    <- nCombinations (n-1) xs'
                                  return (y:ys)                                        






-- Tests & Performance --
--------------------------------------------------------------------------------        

odds_performanceTest = odds 5 [(Card Jack Spades), (Card Queen Clubs)] [(Card Eight Hearts), (Card Four Diamonds)]


import Data.List
import Data.Maybe
import System.IO
import Control.Monad

toCard :: String -> Card
toCard h = Card (toRank h) (toSuit h)

toRank :: String -> Rank
toRank h 
	| c == '2' = Two
	| c == '3' = Three
	| c == '4' = Four
	| c == '5' = Five
	| c == '6' = Six
	| c == '7' = Seven
	| c == '8' = Eight
	| c == '9' = Nine
	| c == 'T' = Ten
	| c == 'J' = Jack
	| c == 'Q' = Queen
	| c == 'K' = King
	| c == 'A' = Ace
	| otherwise = Ace
	where c = head h

toSuit :: String -> Suit
toSuit h
	| c == 'D' = Diamond 
	| c == 'S' = Spade
	| c == 'C' = Club
	| otherwise = Heart
	where c = last h

main = do 
		contents <- readFile "test.txt"
		print . length . filter (==True) . map (showD) . map toHands . map (map toCard) . map words . lines $ contents

toHands :: [Card] -> [[Card]]
toHands [] = []
toHands h = (sortOn rank f) : (toHands l)
	where
		f = take 5 h
		l = drop 5 h


data Card = Card Rank Suit deriving (Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
		deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Suit = Club | Heart | Diamond | Spade deriving (Eq, Show)

data ShowDown = HighCard Rank Rank Rank Rank Rank | 
				OnePair Rank Rank Rank Rank | 
				TwoPair Rank Rank Rank | 
				Set Rank | 
				Straight Rank | 
				Flush Rank Rank Rank Rank Rank |
				FullHouse Rank Rank| 
				Quads Rank | 
				StraightFlush Rank | 
				RoyalFlush
				deriving (Eq, Ord, Show, Read) 

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

straightRank :: ShowDown -> Rank
straightRank (Straight r) = r

showD :: [[Card]] -> Bool
showD a = as > bs 
	where 
		as = showDown $ head a
		bs = showDown $ last a 

showDown :: [Card] -> ShowDown
showDown c = fromJust $ head $ filter (isJust) $ map ($ c) s 
	where s =[royalFlush, straightFlush, quads, fullHouse, flush, straight, trips, twoPair, onePair, highCard]
	

flush :: [Card] -> Maybe ShowDown
flush (a:b:c:d:e:fs) 
	| (length . nub $ map (suit) l) == 1 	= Just $ Flush (rank a) (rank b) (rank c) (rank d) (rank e)
	| otherwise 							= Nothing
	where l = (a:b:c:d:e:fs)

straight :: [Card] -> Maybe ShowDown 
straight h
	| f < Jack && (l == (take 5 $ iterate (succ) f)) = Just $ Straight f
	| otherwise 									 = Nothing
	where l = map (rank) h
	      f = head l 

straightFlush :: [Card] -> Maybe ShowDown
straightFlush h
	| (isJust $ flush h) && (isJust $ r)	= Just $ StraightFlush $ straightRank $ fromJust r
	| otherwise 							= Nothing 
	where r = straight h

royalFlush :: [Card] -> Maybe ShowDown
royalFlush h
	| (isJust $ flush h) && (isJust $ straightFlush h) && l == Ten	= Just RoyalFlush
	| otherwise 													= Nothing
	where l = rank $ head h

quads :: [Card] -> Maybe ShowDown
quads h
	| a == 2 && ((take 4 l) == (take 4 $ repeat $ head l))	= Just $ Quads $ head l
	| a == 2 && ((drop 1 l) == (take 4 $ repeat $ last l))	= Just $ Quads $ last l
	| otherwise												= Nothing  
	where 
		l = map rank h
 		a = length $ nub l 

fullHouse :: [Card] -> Maybe ShowDown
fullHouse h 
	| (length $ nub l) == 2 && (not $ isJust $ quads h) 	= Just $ FullHouse (last l) (head l)
	| otherwise												= Nothing 
	where l = map rank h

trips :: [Card] -> Maybe ShowDown
trips h 
	| a && (take 3 l) == (take 3 $ repeat b) 		= Just $ Set b 
	| a && (take 3 $ drop 1 l) == (take 3 $ repeat m) = Just $ Set m
	| a && (take 3 $ drop 2 l) == (take 3 $ repeat t) = Just $ Set t
	| otherwise 									= Nothing
	where 
		l = map rank h
		a = (length $ nub l) == 3
		t = last l
		m = head $ tail l
		b = head l

twoPair :: [Card] -> Maybe ShowDown
twoPair h 
	| ((length g == 3) && (not $ isJust $ trips h)) = Just $ TwoPair (fromJust p2) (fromJust p1) n
	| otherwise 									= Nothing
	where 
		l = map rank h
		g = group l
		p1 = getPair l
		p2 = getPair $ reverse l
		n = head $ concat $ filter ((==1).length) g
	
getPair :: [Rank] -> Maybe Rank	
getPair [] 		= Nothing
getPair (r:s:rs)
	| r == s 	= Just r
	| otherwise = getPair (s:rs)

onePair :: [Card] -> Maybe ShowDown
onePair h
	| (length g) == 4 	= Just $ OnePair (fromJust p) c b a 
	| otherwise			= Nothing
	where 
		l = map rank h
		g = group l
		p = getPair l
		n = concat $ filter ((==1).length) g
		a = head n
		b = head $ drop 1 n
		c = last n

highCard :: [Card] -> Maybe ShowDown
highCard h 
	| (length g) == 5 	= Just $ HighCard e d c b a
	| otherwise 		= Nothing
	where 
		l = map rank h
		g = group l
		n = concat $ filter ((==1).length) g
		a = head n
		b = head $ drop 1 n
		c = head $ drop 2 n
		d = head $ drop 3 n
		e = last n

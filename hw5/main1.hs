{-# LANGUAGE TupleSections #-}
module Main1 where

-- Larionov Nikita R3496

import Control.Monad.Reader
import Data.Function
import Data.List
import Control.Applicative
import Control.Arrow (first)

data Color = Black | White deriving (Show, Eq)
type SquareId = (Int, Int)
data PieceType = King | Rook deriving (Show, Eq)
type Piece = ((Color, PieceType), SquareId)
type Board = [Piece]
type BoardVariety = [Board]
data Status = Checkmate | Stalemate | Check | Meh deriving (Show, Eq)

dif :: Color -> Color
dif Black = White
dif White = Black

-- movement rules for pieces
movePiece :: PieceType -> SquareId -> [SquareId]
movePiece King (x,y) = [(x+1,y), (x,y+1), (x+1,y+1), (x-1,y+1),(x-1,y), (x,y-1), (x-1,y-1), (x+1,y-1)]
movePiece Rook (x,y) = concat [[(x+i,y),(x-i,y),(x,y+i),(x,y-i)] | i <- [1..8]]

-- check if Piece is inside chess board
onBoard :: SquareId -> Bool
onBoard (x,y) = on (&&) (flip elem [1..8]) x y

-- get all possible positions of Piece after 1 turn
move :: Board -> Piece -> [Piece]
move ys ((c, t), p) = ((c, t),) <$> filter (\x -> onBoard x && (find (\(_, y) -> y == x) ys == Nothing)) (movePiece t p)

makeTurnImpl :: Color -> Board -> Board -> BoardVariety
makeTurnImpl _ [] _ = []
makeTurnImpl c (x@((c', _), _) : xs) ys = if c == c' 
    then map (: xs) (move ys x) ++ map (x :) (makeTurnImpl c xs ys) 
    else map (x :) (makeTurnImpl c xs ys)

-- get all possible configurations of Board after 1 turn (1 color only)
makeTurn :: Color -> Board -> BoardVariety
makeTurn c xs = makeTurnImpl c xs xs

isSubset :: [Piece] -> [Piece] -> Bool
isSubset xs ys = xs == (intersect xs ys)

-- fst arg: if king is "is reached" by current turn
-- snd arg: if king is "is reached" by next turn
getStatusImpl :: Bool -> Bool -> Status
getStatusImpl False False   = Meh
getStatusImpl False True    = Check
getStatusImpl True  False   = Stalemate
getStatusImpl True  True    = Checkmate

-- get game status for player with color "c"
getStatus :: Color -> Board -> Status
getStatus c bs = getStatusImpl (x) (elem (snd k) ys) where
    (Just k) = find ((== (c, King)) . fst) bs
    x        = and $ (flip elem ys) <$> snd <$> (move bs k)
    ys       = snd <$> ((filter ((== c) . dif . fst . fst) bs) >>= (move bs))

configuration :: Board
configuration = [((White, King), (1,3)), ((White, Rook), (8,8)), ((Black, King), (1,1))]

type GameStatus = (Color, BoardVariety)

-- State Monad is used
-- Reader Monad seems more logic, because GameStatus == Environment
-- GameStatus also gets bigger and bigger which should not be the case for state monad
-- In fact, "changing color every turn" and "possibility to make functions more elegant"
-- is not the case with Reader Monad.
-- For example:
-- 
newtype ChessGame a = ChessGame { run :: GameStatus -> Maybe (a, GameStatus) }

instance Functor ChessGame where
    fmap f (ChessGame g) = ChessGame $ fmap (first f) . g
 
instance Applicative ChessGame where
    pure a = ChessGame $ Just . (a, )
    ChessGame f <*> ChessGame x = ChessGame $ \s1 -> do
        (fv, s2) <- f s1
        (xv, s3) <- x s2
        return (fv xv, s3)

instance Alternative ChessGame where
    empty = ChessGame $ \_ -> empty
    ChessGame a <|> ChessGame b = ChessGame $ \c -> a c <|> b c

isOver :: Status -> Bool
isOver Checkmate = True
isOver Stalemate = True
isOver _ = False

cnt :: Color -> BoardVariety -> Int
cnt c bs = length . filter (\xs -> (isOver $ getStatus c xs) || (isOver $ getStatus (dif c) xs)) $ bs >>= (makeTurn c)

-- discard all boards where game has already ended
discard :: Color -> BoardVariety -> BoardVariety
discard c bs = filter (\xs -> (not . isOver $ getStatus c xs) && (not . isOver $ getStatus (dif c) xs)) $ bs >>= (makeTurn c)

doTurn :: (Color -> [Board] -> Int) -> ChessGame Int
doTurn f = ChessGame impl where
    impl (c, []) = Nothing
    impl (c, bs) = Just (f c bs, (dif c, discard c bs))

exactly :: Int -> ChessGame a -> ChessGame [a]
exactly 0 p = pure []
exactly n p = (:) <$> p <*> (exactly (n - 1) p) 

once :: ChessGame a -> ChessGame [a]
once = exactly 1

twice :: ChessGame a -> ChessGame [a]
twice = exactly 2

thrice :: ChessGame a -> ChessGame [a]
thrice = exactly 3

countMates = (sum) <$> twice (doTurn cnt)

main = print . show $ getStatus Black configuration
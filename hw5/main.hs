{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad.Reader
import Data.Function
import Data.List
import Control.Applicative
import Control.Arrow (first, second)
import Data.Maybe

data Color = B | W deriving (Show, Eq, Ord) -- black and white
data PieceType = K | R deriving (Show, Eq) -- king and rook
type SquareId = (Int, Int)
type Piece = ((Color, PieceType), SquareId) -- tuple is chosen because of functions like find
type Board = [Piece]
data Status = Checkmate | Stalemate | Check | Meh deriving (Show, Eq) -- Meh means nothing
data SquareInfo = IS | ID | DI deriving (Show, Eq) 
-- intersect and same color, intersect and different color, don't intersect

dif :: Color -> Color
dif B = W
dif W = B

-- compare color and position of 2 pieces
moveInfoImpl :: Ordering -> Ordering -> SquareInfo
moveInfoImpl EQ EQ = IS
moveInfoImpl _ EQ = ID
moveInfoImpl _ _ = DI

moveInfo :: Piece -> Piece -> SquareInfo
moveInfo ((c,_),p) ((c',_),p') = moveInfoImpl (compare c c') (compare p p')

-- check if Piece intersects with another Piece on the board
pieceIntersect :: Board -> Piece -> SquareInfo
pieceIntersect [] _ = DI
pieceIntersect (b : bs) p = if info /= DI then info else pieceIntersect bs p where
    info = moveInfo b p

-- Piece can't move to the place where Piece with the same color stands
discard :: Board -> [Piece] -> [Piece]
discard bs xs = filter ((/= IS) . pieceIntersect bs) xs

-- special case for rook, bishop and queen
-- we move in 4 different directions until we meet another Piece
moveWhile :: Board -> [Piece] -> [Piece]
moveWhile _ [] = []
moveWhile bs (x : xs) = case pieceIntersect bs x of
    DI -> x : moveWhile bs xs
    ID -> [x]
    IS -> []

-- check if Piece fits color
isColored :: Color -> Piece -> Bool
isColored c ((c',_),_) = c == c'

-- Erasing King from board (special case needed for later)
removeKing :: Color -> Board -> (Piece, Board)
removeKing c (x@((c',t), _) : xs) = if c == c' && t == K
    then (x, xs)
    else second (x :) $ removeKing c xs

-- Check if king can be killed this turn
-- ts is a list of enemy's King SquareIds (special case)
-- We can't do makeTurnImpl for king, because it will use `isKilled` function again
-- Infinite recursion happens:((
-- Basically, we delete King with color `c` and get all possible enemy's positions
-- Then we check that any enemy doesn't intersect with King
isKilled :: Color -> Board -> Bool
isKilled c bs = pieceIntersect ys y == ID where
    dc = dif c
    (y, nbs) = removeKing c $ filter (isColored c) bs
    (x, as) = removeKing dc $ filter (isColored dc) bs
    ts = ((dc,K),) <$> concat (movePieceImpl x)
    ys = (++ ts) . concat $ makeTurnImpl (dif c) as nbs

-- The same algorithm as above, but for list of `possible king positions`
-- This function calls when King moves and checks for legal moves
-- Because King can't move to the square with `checkmate status`
kingFool :: Board -> [Piece] -> [Piece]
kingFool bs xs@(((c,_),_) : _) = filter ((/= ID) . pieceIntersect ys) xs where
    dc = dif c
    (_, nbs) = removeKing c bs
    (x, as) = removeKing dc $ filter (isColored dc) bs
    ts = ((dc,K),) <$> concat (movePieceImpl x)
    ys = (++ ts) . concat $ makeTurnImpl (dif c) as nbs

-- generalization of functions above (could be done in one line, I know)
rqbFilter :: Board -> (Color, PieceType) -> [[SquareId]] -> [SquareId]
rqbFilter bs c@(_,R) = concat . fmap (fmap snd . moveWhile bs . fmap (c,))
rqbFilter bs c@(_,K) = concat . fmap (fmap snd . kingFool bs . fmap (c,))

-- basic rules for Pieces' movement
movePieceImpl :: Piece -> [[SquareId]]
movePieceImpl ((_,K),(x,y)) = [[(x+1,y), (x,y+1), (x+1,y+1), (x-1,y+1), (x-1,y), (x,y-1), (x-1,y-1), (x+1,y-1)]]
movePieceImpl ((_,R),(x,y)) = [[(x+i,y) | i <- [1..8]]
                              ,[(x-i,y) | i <- [1..8]]
                              ,[(x,y+i) | i <- [1..8]]
                              ,[(x,y-i) | i <- [1..8]]]

movePiece :: Board -> Piece -> [Piece]
movePiece bs x@(p,_) = discard bs $ (p,) <$> rqbFilter bs p (movePieceImpl x)

onBoard :: SquareId -> Bool
onBoard (x,y) = on (&&) (flip elem [1..8]) x y

-- get all possible Piece's movements for board configuration
move :: Board -> Piece -> [Piece]
move bs p@(c,_) = do
    (_,pos) <- movePiece bs p
    guard (onBoard pos)
    return (c,pos)

-- make Turn for all Pieces with same color
makeTurnImpl :: Color -> Board -> Board -> [Board]
makeTurnImpl _ [] _ = []
makeTurnImpl c (x@((c', _), _) : xs) ys = if c == c' 
    then map (: xs) (move ys x) ++ map (x :) (makeTurnImpl c xs ys) 
    else map (x :) (makeTurnImpl c xs ys)

makeTurn :: Color -> Board -> [Board]
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

getStatus :: Color -> Board -> Status
getStatus c bs = getStatusImpl (isKilled c bs) (null suka) where
    (Just k) = find ((== (c, K)) . fst) bs
    suka = move bs k

type GameStatus = (Color, [Board])

-- State Monad is used
-- Reader Monad seems more logic, because GameStatus == Environment
-- GameStatus also gets bigger and bigger which should not be the case for state monad
-- In fact, "changing color every turn" does not fit into reader monad
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

-- Count all `end game` situations
cnt :: Color -> [Board] -> Int
cnt c bs = length . filter (\xs -> (isOver $ getStatus c xs) || (isOver $ getStatus (dif c) xs)) $ bs >>= (makeTurn c)

-- discard all boards where game has already ended
discard' :: Color -> [Board] -> [Board]
discard' c bs = filter (\xs -> (not . isOver $ getStatus c xs) && (not . isOver $ getStatus (dif c) xs)) $ bs >>= (makeTurn c)

doTurn :: (Color -> [Board] -> Int) -> ChessGame Int
doTurn f = ChessGame impl where
    impl (c, []) = Nothing
    impl (c, bs) = Just (f c bs, (dif c, discard' c bs))

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

-- simple example: (W, _) means that W is gonna make turn first
configuration :: Board
configuration = [((W, K), (2,3)), ((W, R), (8,8)), ((B, K), (1,1))]

main = print $ fromJust $ fmap (sum . fst) $ run (thrice countMates) (W, [configuration])
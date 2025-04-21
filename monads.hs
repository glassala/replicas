{-
    Attempted functional replicas of Haskell's Maybe, State, and linked list.
-}
import Control.Applicative
import Data.Bits
{-
    "Maybe" analogue.
-}
data Perhaps a = Present a | Absent
    deriving Show
instance Functor Perhaps where
    fmap :: (a -> b) -> Perhaps a -> Perhaps b
    fmap f q = case q of
        Absent -> Absent
        Present n -> Present $ f n 
instance Applicative Perhaps where
    pure :: a -> Perhaps a
    pure q = Present q
    (<*>) :: Perhaps (a -> b) -> Perhaps a -> Perhaps b
    Absent <*> q = case q of
        Absent -> Absent
        Present _ -> Absent
    Present f <*> q = case q of
        Absent -> Absent
        Present n -> Present $ f n
instance Monad Perhaps where
    (>>=) :: Perhaps a -> (a -> Perhaps b) -> Perhaps b
    q >>= f = case q of
        Absent -> Absent
        Present n -> f n
instance Alternative Perhaps where
    empty :: Perhaps a
    empty = Absent
    (<|>) :: Perhaps a -> Perhaps a -> Perhaps a
    Absent <|> x = x
    Present x <|> _ = Present x
{-
    "State" analogue.
-}
newtype Status s a = Status {
    advance :: s -> (a, s)
}
instance Functor (Status s) where
    fmap :: (a -> b) -> Status s a -> Status s b
    fmap f st = Status $ \s ->
        let (a, s') = advance st s
        in (f a, s')
instance Applicative (Status s) where
    pure :: a -> Status s a
    pure x = Status $ \s -> (x, s)
    (<*>) :: Status s (a -> b) -> Status s a -> Status s b
    stf <*> stn = Status $ \s -> 
        let (f, s') = advance stf s
            (n, s'') = advance stn s'
        in (f n, s'')
instance Monad (Status s) where
    (>>=) :: Status s a -> (a -> Status s b) -> Status s b
    st >>= f = Status $ \s ->
        let (a, s') = advance st s
        in advance (f a) s'
{-
    Get.
-}
retrieve :: Status s s
retrieve = Status $ \st -> (st, st)
{-
    Put.
-}
specify :: s -> Status s ()
specify st = Status $ \_ -> ((), st)
{-
    Modify.
-}
alter :: (s -> s) -> Status s ()
alter f = Status $ \st -> ((), f st)
{-
    EvalState.
-}
evaluate :: Status s a -> s -> a
evaluate st sf = fst $ advance st sf
{-
    ExecState.
-}
execute :: Status s a -> s -> s
execute st sf = snd $ advance st sf
{- 
    Singly linked list.
-}
data List a = Nil | a :+ List a
    deriving (Eq, Ord)
infixr 5 :+ --Sets right associativity and operator precedence of 5 (on a scale 1-9) for :+
{-
    Provides the string representation of a List.
-}
instance Show a => Show (List a) where
    showsPrec :: 
        Show a =>
        Int -> List a -> ShowS
    showsPrec _ Nil = showString "[]"
    showsPrec _ (head :+ tail) = showChar '[' . shows head . showRest tail where
        showRest Nil = showChar ']'
        showRest (first :+ rest) =
            showChar ',' . showChar ' ' . shows first . showRest rest
{-
    List reversal.
-}
rev :: List a -> List a
rev Nil = Nil
rev k = f k Nil where
    f Nil reversed = reversed
    f (head :+ tail) reversed = f tail (head :+ reversed)
{-
    Semigroup instance. A semigroup is a set/type with an associative operation.
-}
instance Semigroup (List a) where
    (<>) :: List a -> List a -> List a -- List concatenation.
    Nil <> a = a
    (head :+ tail) <> chain = head :+ (tail <> chain)
{-
    A monoid is a semigroup with an identity element.
-}
instance Monoid (List a) where
    mempty :: List a
    mempty = Nil
{-
    A functor is a type where functions can be "lifted" into the type's context.
-}
instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (head :+ tail) = f head :+ fmap f tail
{-
    [f, g] <*> [a, b] = [f a, f b, g a, g b]
-}
instance Applicative List where
    pure :: a -> List a
    pure item = item :+ Nil
    (<*>) :: List (a -> b) -> List a -> List b
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (fhead :+ ftail) <*> chain = (fhead <$> chain) <> (ftail <*> chain)
{- 
    Adds a bind operation. For lists, monadic bind is essentially flatmap.
-}
instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    Nil >>= f = Nil
    (head :+ tail) >>= f = (f head) <> (tail >>= f)
{- 
    "Folds" items in a list from the right (or left) according to a function.
-}
instance Foldable List where
    foldr :: (a -> b -> b) -> b -> List a -> b
    foldr f folded Nil = folded
    foldr f folded (head :+ tail) = foldr f (f head folded) tail
    foldl :: (b -> a -> b) -> b -> List a -> b
    foldl f folded Nil = folded
    foldl f folded (head :+ tail) = foldl f (f folded head) tail
{- 
    Filter analogue.
-}
sift ::
    (a -> Bool) -> List a -> List a
sift _ Nil = Nil
sift characteristic (head :+ tail)
    | characteristic head = head :+ sift characteristic tail
    | otherwise = sift characteristic tail
{-
    Naive quicksort.
-}
qsort :: 
    Ord a =>
    List a -> List a
qsort Nil = Nil
qsort (head :+ tail) = 
    qsort (sift (\y -> y <= head) tail) <>
    (head :+ Nil) <>
    qsort (sift (\y -> y > head) tail)
{-
    Checks to see if a list is empty or not.
-}
emptiness :: List a -> Bool
emptiness chain = case chain of
    Nil -> True
    _ -> False
{-
    List length.
-}
len :: List a -> Int
len = foldr (\_ count -> count + 1) 0
{-
    Returns the head of a list if it exists.
-}
headOf :: List a -> Perhaps a
headOf Nil = Absent
headOf (head :+ _) = Present head
{-
    Returns the tail of a list if it exists.
-}
tailOf :: List a -> Perhaps (List a)
tailOf Nil = Absent
tailOf (_ :+ tail) = Present tail
{-
    Take n elements.
-}
extract :: Int -> List a -> List a
extract n _ | n <= 0 = Nil
extract n Nil = Nil
extract n (head :+ tail) = head :+ extract (n - 1) tail
{-
    Drop n elements.
-}
discard :: Int -> List a -> List a
discard n chain | n <= 0 = chain
discard n Nil = Nil
discard n (head :+ tail) = discard (n - 1) tail
{-
    Turns a list of lists into a single long list.
-}
spool :: List (List a) -> List a
spool Nil = Nil
spool (chain :+ heap) = chain <> spool heap
{-
    Zips two lists.
-}
entwine :: List a -> List b -> List (a, b)
entwine Nil _ = Nil
entwine _ Nil = Nil
entwine (xhead :+ xtail) (yhead :+ ytail) =
    (xhead, yhead) :+ entwine xtail ytail
{-
    Checks if a list contains an object.
-}
contains :: 
    Eq a =>
    List a -> a -> Bool
Nil `contains` needle  = False
(hay :+ stack) `contains` needle  = case (needle == hay) of
    True -> True
    False -> stack `contains` needle
{-
    Extracts the first instance of an element which satisfies
    a characteristic from a list, if such an element exists.
-}
seekout :: (a -> Bool) -> List a -> Perhaps a
seekout _ Nil = Absent
seekout characteristic (candidate :+ pool) =
    case (characteristic candidate) of
        True -> Present candidate
        False -> seekout characteristic pool
{-
    Removes duplicates from a list.
-}
dedupl :: 
    Eq a =>
    List a -> List a
dedupl = act Nil where
    act _ Nil = Nil
    act seen (head :+ tail)
        | seen `contains` head = act seen tail
        | otherwise = head :+ act (head :+ seen) tail
{-
    Miscellaneous tangentially list-related functions ~
-}
{-
    Simple XORshift RNG.
-}
rng :: 
    Integral a =>
    Bits a => 
    a -> a -> a -> List a
rng seed 0 _ = Nil
rng seed count modulus | seed < 0 = rng (abs seed) count modulus
rng seed count modulus =
    (process `mod` modulus) :+ rng process (count - 1) modulus where
        process = 
            xorshiftL 17 .
            xorshiftR 7 .
            xorshiftL 13 $ seed where
                xorshiftL n s = s `xor` (s `shiftL` n)
                xorshiftR n s = s `xor` (s `shiftR` n)
{-
    Scales/normalizes a list of integers.
-}
normalize :: 
    Integral a => 
    Fractional b =>
    List a -> b -> List b
normalize chain factor = 
    (/ factor) <$> ((fromIntegral) <$> chain)
{-
    Counts instances of an item in a list.
-}
countInst ::
    (Eq a) => 
    a -> List a -> Int
countInst item Nil = 0
countInst item (head :+ tail) = x + countInst item tail where
    x = if item == head then 1 else 0
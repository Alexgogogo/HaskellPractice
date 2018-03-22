-- 1.returns a new list that is the same as lst, except x has been added to the end of it
snoc :: a -> [a] -> [a]
snoc x []           = [x]
snoc x (first:rest) = first: (snoc x rest)



-- 2.Write your own version of the Haskell append operator ++
myappend :: [a] -> [a] -> [a]
myappend lst []           = lst
myappend lst (first:rest) = myappend (snoc first lst) rest



-- 3.Write your own version of reverse
myreverse :: [a] -> [a]
myreverse lst = myreverseHelper lst []

-- helper function for myreverse
myreverseHelper :: [a] -> [a] -> [a]
myreverseHelper [] lst = lst
myreverseHelper (first:rest) lst = myreverseHelper rest (first:lst)



-- 4.returns the number of emirps less than, or equal to, n
count_emirps :: Int -> Int
count_emirps n
             | n < 13    = 0
             | otherwise = if (is_emirps n) == True then 1 + (count_emirps (n - 1)) else (count_emirps (n - 1))
             
-- find prime numbers
-- retrieved from http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html
smallest_divisor :: Int -> Int
smallest_divisor n
                 | n < 0     = error "n must be >= 0"
                 | n == 0    = 0
                 | n == 1    = 1
                 | otherwise = head (dropWhile(\x -> n `mod` x /=0)[2..n])
                 
is_prime :: Int -> Bool
is_prime  n
          | n < 2     = False
          | otherwise = (smallest_divisor n) == n
          
-- retrieved from Haskell Documentation          
reverse_n :: Int -> Int
reverse_n n = read . reverse . show $ n  

is_prime_reverse :: Int -> Bool
is_prime_reverse n 
                 | n < 10    = False
                 | otherwise = (smallest_divisor (reverse_n n)) == (reverse_n n)

is_emirps :: Int -> Bool
is_emirps n = (is_prime n) && (is_prime_reverse n)



-- 5. takes a list of one, or more, integer lists as input, and returns the list with the greatest sum
biggest_sum :: [[Int]] -> [Int]
biggest_sum [lst]                                                          =  lst
biggest_sum (first:rest)
                        | (find_sum first) > (find_sum (biggest_sum rest)) = first
                        | otherwise                                        = (biggest_sum rest)
                         

-- find sum of a list
find_sum :: [Int] -> Int
find_sum []           = 0
find_sum (first:rest) = first + (find_sum rest)



-- 6. returns the item in seq that maximizes function f
greatest :: (a -> Int) -> [a] -> a
greatest f [lst]                                        = lst
greatest f (first:rest)
                     |(f first) > (f (greatest f rest)) = first
                     |otherwise                         = (greatest f rest)



-- 7.  returns True when x is 0 or 1, and False otherwise.
is_bit :: (Eq a, Num a) => a -> Bool
is_bit x
       | x == 0 || x==1 = True
       | otherwise      = False



-- 8. returns 1 if x is 0, and 0 if x is 1. If x is not a bit, then call error msg, where msg is a helpful error message string.
flip_bit :: (Eq a, Num a, Num b) => a -> b
flip_bit x
         | x == 0    = 1 
         | x == 1    = 0
         | otherwise = error " x is not a bit"
         
         
         
-- 9(a) returns True if x is the empty list, or if it contains only bits (as determined by is_bit).
is_bit_seq1 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq1 [ ]                          = True
is_bit_seq1 (first:rest) 
                        | (is_bit first) = (is_bit_seq1 rest)
                        | otherwise      = False

-- 9(b) 
is_bit_seq2 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq2 [ ]          = True
is_bit_seq2 (first:rest) = if (is_bit first) then (is_bit_seq2 rest) else False

-- 9(c)
is_bit_seq3 :: (Eq a, Num a) => [a] -> Bool
is_bit_seq3 [ ] = True
is_bit_seq3 lst = (filter (\x -> x==0 || x==1) lst) == lst 



-- 10(a) returns a sequence of bits that is the same as x, except 0s become 1s and 1s become 0s
invert_bits1 :: (Eq a, Num a) => [a] -> [a]
invert_bits1 [ ]        = [ ]
invert_bits1 (first:rest)
                        | first == 0 = myappend [1] (invert_bits1 rest)
                        | first == 1 = myappend [0] (invert_bits1 rest)
                        | otherwise  = error "list is not valid"


-- 10(b)
invert_bits2 :: (Eq a, Num a) => [a] -> [a]
invert_bits2 x = map (\y -> flip_bit y) x

-- 10(c)
invert_bits3 :: (Eq a, Num a) => [a] -> [a]
invert_bits3 x = [flip_bit b| b <- x ]

-- 11. returns a pair of values indicating the number of 0s and 1s in x
bit_count :: [Int] -> (Int , Int)
bit_count [ ] = (0,0)
bit_count lst = (length (filter (\x -> x == 0) lst),length (filter (\x -> x == 1) lst))



-- 12. returns a list of all bit sequences of length n
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n 
                   | n < 1     = [] 
                   | otherwise = (bits_sequence n p)
                       where  p = (n^2 - 1)
                       
trim_bits :: [Int]->[Int]
trim_bits lst n

bits_sequence :: Int -> Int -> [[Int]]
bits_sequence n (-1) = []
bits_sequence n k    = [decimal_to_bits(k)] ++ (bits_sequence n (k - 1))

decimal_to_bits :: Int -> [Int]
decimal_to_bits 0 = [0] 
decimal_to_bits n = decimal_to_bits(n `quot` 2) ++ [n `mod` 2]

data List a = Empty | Cons a (List a)
    deriving Show
-- 13. converts a regular Haskell list to a List a
toList :: [a] -> List a
toList [ ]  = Empty
toList (first:rest) = Cons first (toList rest)



-- 14. converts a List a to a regular Haskell list
toHaskellList :: List a -> [a]
toHaskellList Empty = [ ]
toHaskellList (Cons a b)  = a: toHaskellList b



-- 15. returns a new List a that consists of all the elements of A followed by all the elements of B
append :: (Eq a) => List a -> List a -> List a
append Empty lst      = lst
append (Cons a b) lst = Cons a $ (append b lst)



-- 16. returns a List a that is the same as L but all items satisfying f (i.e. for which f returns True) have been removed
removeAll :: (a -> Bool ) -> List a -> List a
removeAll f Empty      = Empty
removeAll f (Cons a b) = if (f a) == True then (removeAll f b) else Cons a $ (removeAll f b)



-- 17. returns a new List a that is a sorted version of L
sort :: Ord a => List a -> List a
sort (Cons a b) = quicksort(Cons a b)

-- retrieved from http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html
quicksort :: Ord a => List a-> List a
quicksort Empty      = Empty
quicksort (Cons a b) = (append(append bigs (Cons a Empty)) smalls)
                       where smalls = quicksort (removeAll (<= a) b)
                             bigs   = quicksort (removeAll (>  a) b)
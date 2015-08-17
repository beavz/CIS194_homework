-- UPenn CIS194 | Spring '13
-- Homework 1 | KRB

-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1       = []
  | n < 10      = [n]
  | otherwise   = n `mod` 10:toDigitsRev (n `quot` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther a
  | (length a) `mod` 2 == 0  = evenDoubleEveryOther a
  | otherwise              = oddDoubleEveryOther a

evenDoubleEveryOther :: [Integer] -> [Integer]
evenDoubleEveryOther []     = []
evenDoubleEveryOther (a:(b:cs)) = (2*a) : ( b : (evenDoubleEveryOther cs))

oddDoubleEveryOther :: [Integer] -> [Integer]
oddDoubleEveryOther [] = []
oddDoubleEveryOther (a:as) = a : (evenDoubleEveryOther as)

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [a]
  | a < 10    = a
  | otherwise = sumDigits ( toDigits a )
sumDigits (a:as) = (sumDigits [a]) + (sumDigits as)

-- Exercise 4

validate :: Integer -> Bool
validate x
  | ( crazySum x ) `mod` 10 == 0 = True
  | otherwise                    = False

crazySum x = sumDigits ( doubleEveryOther( toDigits x ) )

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [ Move ]
hanoi n a b c
  | n == 1 = [ (a, c) ]
  | n > 1  = ( hanoi (n - 1) a c b ) ++ ( (a, c) : ( hanoi (n - 1) b a c ) )

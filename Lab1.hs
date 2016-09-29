module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

--Ex1. testing induction for squares

square :: Int -> Int
square x = x*x

sumlist2:: Int -> Int
sumlist2  m = sum (map square [x | x<- [1..m]])

formula2:: Int -> Int
formula2 n = div (n*(n+1)*(2*n+1)) 6

test2 :: Int -> Bool
test2 n = if n<0 then True else sumlist2 n == formula2 n

cubed :: Int -> Int
cubed x = x*x*x

sumlist3:: Int -> Int
sumlist3 m = sum (map cubed [x | x <- [1..m]])

formula3:: Int -> Int
formula3 n = square (div (n*(n+1)) 2)

test3 :: Int -> Bool
test3 n = if n<0 then True else sumlist3 n  == formula3 n

--Ex2 test property powerset

power1:: Int -> Int
power1 n = 2 ^ n

test4 :: Int -> Bool
test4 n = if n<0 then True else length (subsequences [1..n]) == power1 n

--Ex3 Test permutations counter & factorial

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
 insrt x [] = [[x]]
 insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial n = if n == 0 then 1 else n * factorial (n - 1)

test5:: Int-> Bool
test5 n= if n<0 then True else length(perms[1..n]) == factorial n

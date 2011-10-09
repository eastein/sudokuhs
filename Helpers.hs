--some basic data manipulation code from http://www.iayc.org/forum/viewtopic.php?p=8409&sid=078b42578b3502999e16cffea8305e4f
--not AI focused, used as a library
--additions: splitl, eatlinebreak, nextlinebreak, metasplit, diff, distinct, intersect, naturals

module Helpers (tokenize, toint, naturals, splitl, metasplit, nseq, diff, distinct, intersect, qsort, sort, insert, pad) where
import Data.Char

qsort pred []     = []
qsort pred (x:xs) = qsort pred (filter (\p -> (pred p) < (pred x)) xs) ++ [x] ++ qsort pred (filter (\p -> (pred p) >= (pred x)) xs)

sort l = qsort (\i -> i) l

distinct :: (Eq a) => [a] -> [a]
distinct l = distinct_ [] l

distinct_ :: (Eq a) => [a] -> [a] -> [a]
distinct_ l (p:lr) = (diff [p] l) ++ (distinct_ (p:l) lr)
distinct_ l [] = []

diff :: (Eq a) => [a] -> [a] -> [a]
diff a b = filter (\x -> not (inn x b)) a

intersect :: (Eq a) => [[a]] -> [a]
intersect (f:r) = filter (\x -> all (\c -> inn x c) r) f
intersect [] = []

insert i e l = ins 0 i e l
ins n d e (f:r) = if ((mod n d) == 0) then
		  	e:f:(ins (n + 1) d e r)
		  else
		  	f:(ins (n + 1) d e r)
ins n d e [] = []

pad n s = let cl = length s in
                (take (n - cl) (cycle " ")) ++ s

inn :: (Eq a) => a -> [a] -> Bool
inn b (a:as) = if (a==b) then True else inn b as
inn a b = False

nseq n = iterate (\x -> x + 1) n

naturals = iterate (\x -> x + 1) 1

int :: Char->Int
int a = case a of
   '0' -> 0
   '1' -> 1
   '2' -> 2
   '3' -> 3
   '4' -> 4
   '5' -> 5
   '6' -> 6
   '7' -> 7
   '8' -> 8
   '9' -> 9

metasplit :: (Eq a) => a -> [a] -> [[a]]
metasplit s l = metasplit_ s [] l

metasplit_ :: (Eq a) => a -> [a] -> [a] -> [[a]]
metasplit_ s pl (h:t) | h == s = (reverse pl):(metasplit_ s [] t)
                      | otherwise = metasplit_ s (h:pl) t
metasplit_ s pl [] = [reverse pl]

eatlinebreak :: String -> String
eatlinebreak (a:as) = if a==' ' then eatlinebreak as else (a:as)
eatlinebreak a = a

nextlinebreak :: String -> String -> (String, String)
nextlinebreak (a:as) b = if (a=='\n') then (b,as) else nextlinebreak as (b++(a:[]))
nextlinebreak a b = (b,a)

splitl :: String -> [String]
splitl [] = []
splitl a = x:splitl y where
   (x,y)=nextlinebreak (eatlinebreak a) ""

eatspace :: String -> String
eatspace (a:as) = if a==' ' then eatspace as else (a:as)
eatspace a = a

nextspace :: String -> String -> (String, String)
nextspace (a:as) b = if (a==' ') then (b,as) else nextspace as (b++(a:[]))
nextspace a b = (b,a)

tokenize :: String -> [String]
tokenize [] = []
tokenize a = x:tokenize y where
   (x,y)=nextspace (eatspace a) ""

numeric :: String -> Bool
numeric (a:as) = if inn a numbers then numeric as else False
   where numbers = ['0'..'9']
numeric a = True

toint s = toint_ (reverse s)

toint_ (a:as) = ((int a) + (10* (toint_ as)))
toint_ [] = 0

numerize ::  [String]-> String
numerize (a:b:c:cs) =  if c=="+" then add else mult where
   add=numerize (show ((toint a)+ (toint b)):cs)
   mult=numerize (show ((toint a)* (toint b)):cs)
numerize (a:as) = a

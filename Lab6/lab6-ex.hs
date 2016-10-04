{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List
import Debug.Trace
import TestPP
import Data.List.Split
{-
Expresia `undefined` are orice tip dar nu poate fi evaluată.
-}

{-
1. (1p)
Construiți funcții simple pentru următoarele tipuri (completați definițiile):
-}
identity :: a -> a
identity a = a

notF :: Bool -> Bool
notF True = False
notF _    = True

pair1 :: (a, b) -> a
pair1 (a, undefined) = a

-- Verificare: check1
check1 :: TestPP ()
check1 = do
  assertVal "[1] pair1 (notF False, undefined)" 0.5 $ -- 0.5p
    pair1 (notF False, undefined)
  assertProp "[1] pair1 (x, undefined)" 0.5 -- 0.5p
    (\ x -> x == pair1 (x, undefined)) 42

{-
2. (2p)
Implementați funcția `unzip2`
-}
unzip2  :: [(a, b)] -> ([a], [b])
unzip2 [] = ([], [])
unzip2 x = (map fst x, map snd x)

-- Verificare: check2
check2 :: TestPP ()
check2 = do
  assertVal "[2] unzip2 (zip)" 2 $ -- 2p
    unzip2 (zip [1,2,3] ["a","b","c"]) == ([1,2,3], ["a","b","c"])

{-
3. (2p)
Implementați, folosind obligatoriu list-comprehensions, funcționala, `map`.
-}
mapLC :: (a -> b) -> [a] -> [b]
mapLC f list = [f x | x <- list]

-- Verificare: check3
check3 :: TestPP ()
check3 = do
  assertVal "[3] mapLC (* 2)" 1 $ -- 1p
    mapLC (* 2) [1,2,3] == [2,4,6]
  assertProp "[3] mapLC (\\ x -> [x, not x])" 1 -- 1p
    (\ xs -> mapLC (\ x -> [x, not x]) xs == map (\ x -> [x, not x]) xs)
    [True, False, True]

{-
4. (1p)
Implementați, folosind obligatoriu list-comprehensions, funcționala `filter`.
-}
filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f list = [x | x <- list, f x]

-- Verificare: check4
check4 :: TestPP ()
check4 = do
  assertVal "[4] filterLC (\\ x -> x `mod` 2 == 0)" 0.2 $ -- 0.2p
    filterLC (\ x -> x `mod` 2 == 0) [1,2,4] == [2,4]
  assertProp "[4] filterLC (\\ x -> x `mod` 2 == 0) again" 0.4 -- 0.4p
    (\ xs -> filterLC (\ x -> x `mod` 2 == 0) xs ==
             filter (\ x -> x `mod` 2 == 0) xs) [1,2,3,4,5,6]
  assertProp "[4] filterLC id" 0.4 -- 0.4p
    (\ xs -> filterLC id xs == filter id xs) [True, True, False, True, True]

{-
5. (1p)
Implementați o funcție ce calculează cmmdc-ul a două numere pozitive.
-}
cmmdc :: Integer -> Integer -> Integer
cmmdc x 0 = x
cmmdc 0 _ = 0
cmmdc x y = cmmdc y $ x `mod` y

-- Verificare: check5
check5 :: TestPP ()
check5 = do
  assertVal "[5] cmmdc 6 x" 0.5 $ -- 0.5p
    and [ cmmdc 6 4 == 2
        , cmmdc 6 12 == 6
        , cmmdc 6 9 == 3
        , cmmdc 6 7 == 1
        ]
  assertVal "[5] cmmdc x y" 0.5 $ -- 0.5p
    (\ x y -> let c = cmmdc x y in and [x `mod` c == 0, y `mod` c == 0]) 4 5

{-
6. (1p)
Folosind list-comprehensions traduceți în Haskell mulțimea,
 unde (x, y) este cmmdc al x și y

  { (a, b, c) | a<- [1..10], b<-[2..10], b > a, c = (a, b)}
-}
multime = [(a, b, c) | a<- [1..10], b<-[2..10], b > a, c <- [cmmdc a b]]

-- Verificare: check6
check6 = do
  let answer = [(1,2,1),(1,3,1),(1,4,1),(1,5,1),(1,6,1),(1,7,1),(1,8,1),(1,9,1),(1,10,1),(2,3,1),(2,4,2),(2,5,1),(2,6,2),(2,7,1),(2,8,2),(2,9,1),(2,10,2),(3,4,1),(3,5,1),(3,6,3),(3,7,1),(3,8,1),(3,9,3),(3,10,1),(4,5,1),(4,6,2),(4,7,1),(4,8,4),(4,9,1),(4,10,2),(5,6,1),(5,7,1),(5,8,1),(5,9,1),(5,10,5),(6,7,1),(6,8,2),(6,9,3),(6,10,2),(7,8,1),(7,9,1),(7,10,1),(8,9,1),(8,10,2),(9,10,1)]
  assertVal "[6] answer == sort multime" 1 $ -- 1p
    answer == sort multime


{-
7. (2p)
Definiți o funcție care întoarce primul cuvânt dintr-un string (cuvintele 
sunt delimitate de un spațiu.
-}
firstWord :: String -> String
firstWord input = head (words input) -- words transforma intr-o lista de cuvinte

-- Verificare: check7
check7 = do
  assertVal "[7] firstWord" 1 $ -- 1p
    firstWord "Ce laborator frumos!" == "Ce"
  assertVal "[7] firstWord, again" 1 $ -- 1p
    firstWord "Ce" == "Ce"

{-
8. (BONUS, 3p)
String substitution. Definiți `subst` care înlocuiește cuvinte, astfel încât:

  subst "frumos" "destept" "Ce laborator frumos !" = "Ce laborator destept !"

Cuvintele sunt delimitate de spațiu.
  
Hint: s-ar putea să aveți nevoie de funcții auxiliare. Puteți folosi `words`
pentru a obține cuvintele din frază și `unwords` sau `++` pentru a obține
frază din cuvinte.
-}
subst :: String -> String -> String -> String
subst what new input = intercalate new (splitOn what input)

-- Verificare: check8
check8 =
  assertVal "[8] subst" 3 $ -- 3p
    subst "frumos" "destept" "Ce laborator frumos !" == "Ce laborator destept !"

{-
9. (BONUS, 2p)
Implementați `increasingPairs` (din `lab6-doc.hs`) astfel încât să se genereze
elementele în ordinea crescătoare a sumei valorilor din pereche.
-}
increasingPairs = [(x, y - x)|  y <- [0..], x <- [0..y], x < y - x]

-- Verificare: check9
check9 =
  let answer = [(0,1),(0,2),(0,3),(1,2),(0,4),(1,3),(0,5),(1,4),(2,3),(0,6)]
  in assertVal "[9] increasingPairs" 2 $
      take 10 increasingPairs == answer

{-
Helpers for testing :)
-}
runAllTests = runTestPP $
  sequence_[check1, check2, check3, check4, check5, check6, check7, check8, check9]

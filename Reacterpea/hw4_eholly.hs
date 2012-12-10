-- Homework 4
-- by Ethan Holly

import System.Random
import Euterpea

test :: Music Pitch
test = note (1/2) (C, 4)

upOct :: Music Pitch -> Music Pitch
upOct (Prim (Note dur (pc, oct))) = (note dur (pc, oct+1))

intsToList :: [Integer] -> Music Pitch -> [Music Pitch]
intsToList integers musicA = map (\integer -> transpose (fromInteger integer) musicA) integers

listToMelody :: [Music Pitch] -> Music Pitch
listToMelody list = foldl (\musicA musicB -> musicA :+: musicB) (rest 0) list

intsToMelody :: [Integer] -> Music Pitch -> Music Pitch
intsToMelody integers music = listToMelody $ intsToList integers music

listToChord :: [Music Pitch] -> Music Pitch
listToChord list = foldl (\musicA musicB -> musicA :=: musicB) (rest 0) list

intsToChord :: [Integer] -> Music Pitch -> Music Pitch
intsToChord integers music = listToChord $ intsToList integers music

type MP = Music Pitch
rptChange :: (MP -> MP) -> Integer -> MP -> MP
rptChange f times music = listToMelody (take (fromInteger times) (iterate f music))

data M = N (Music Pitch) | Seq [M] | Par [M] | Faster Rational M

toMusic :: M -> Music Pitch
toMusic (N mp) = mp
toMusic (Seq list) = case (length list) of
                          0 -> rest 0
                          1 -> toMusic (head list)
                          _   -> (toMusic (head list)) :+: (toMusic (Seq (drop 1 list)))
toMusic (Par list) = case (length list) of
                          0 -> rest 0
                          1 -> toMusic (head list)
                          _   -> (toMusic (head list)) :=: (toMusic (Par (drop 1 list)))
toMusic (Faster rational m) = tempo rational (toMusic m)

instance Num M where
  m1 + m2 = Seq [m1, m2]
  m1 * m2 = Par [m1, m2]

c3, d3, e3, f3, g3 :: M
c3 = N (c 3 1)
d3 = N (d 3 1)
e3 = N (e 3 1)
f3 = N (f 3 1)
g3 = N (g 3 1)

h m = Faster 2 m
q m = Faster 4 m

testNumSyntax = toMusic $ q (c3*e3*g3) + h (d3*e3) + c3*e3 + q (c3 + d3 + e3 + f3 + g3)

data State = State [(Integer, Music Pitch, State)]

totalProb :: State -> Double
totalProb (State tuples) = foldl (\ prob (integer,_,_) -> prob + (fromInteger integer) ) 0.0 tuples

markovChoose :: Double -> [(Integer, a, b)] -> (Integer, a, b)
markovChoose d [x] = x
markovChoose d list = let (integer, a, b):xs = list in
                          if (d <= (fromInteger integer))
                             then (integer, a, b)
							 else markovChoose (d - (fromInteger integer)) xs

markov :: [Double] -> State -> [Music Pitch]
markov randList (State tuples) = let (integer, music, state) = markovChoose (head randList * totalProb(State tuples)) tuples
                                     in music:(markov (drop 1 randList) state)

s1 = State [(1, c 3 1, s1), (2, d 3 2, s2)]
s2 = State [(2, c 3 0.5, s1), (3, f 3 0.5, s2)]

mark st = do
   gen <- newStdGen
   let ns = randoms gen :: [Double]
   play (Modify (Tempo 20) (line (take 50 (markov ns st))))
   
major1 = (c 4 qn) :=: (e 4 qn) :=: (g 4 qn)
major2 = (rest (2/12)) :+: (tempo (3) major1)

minor1 = (c 4 qn) :=: (e 4 qn) :=: (a 4 qn)
minor2 = (rest (2/12)) :+: (tempo (3) minor1)

beat1 = major1 :=: major2
beat2 = minor1 :=: minor2
beats = tempo (3/4) (beat1 :+: beat2)
beats2 = beats :+: beats

sb1 = State [(1, beats2, sb1), (1, transpose 5 beats2, sb2), (1, transpose 7 beats, sb3)]
sb2 = State [(1, beats2, sb1), (1, transpose 5 beats2, sb2)]
sb3 = State [(1, transpose 5 beats, sb3a)]
sb3a = State [(1, beats, sb3b)]
sb3b = State [(1, tempo (1/3) (transpose (-5) major1), sb1)]

blues = do 
   gen <- newStdGen
   let ns = randoms gen :: [Double]
   play $ line (take 24 (markov ns sb1))
module Reacterpea where
import FRP
import SOE
import Euterpea

type BPitch = Behavior Pitch
type BMPitch = Behavior (Music Pitch)

type REvent = FRP.Event

--event utility
revent :: (Stimulus -> (REvent a, Maybe a)) -> REvent a
revent f = FRP.Event f

rkey = FRP.key

now :: REvent ()
now = revent (\s -> (never, Just ()))

--operators
(%*) :: Integral a => Behavior a -> Behavior a -> Behavior a
(%*) = lift2 mod

--relevant loops
pulse :: Double -> REvent Double
pulse r = f r where
	f a = revent (\(t, _) -> case (t > a) of
				True  -> (f (t+r), Just (t))
				False -> (f a, Nothing)
			)
			
metronome rate = wrap 16 (4*rate) where
	tloop tmp = pulse (1/(tmp/60)) --tmp represents beats per minute
	wrap b t = (accum 0 ((tloop t) -=> (+1))) %* (lift0 b)

--notes
a_, as_, bf_, b_, c_, cs_, df_, d_, ef_, e_, f_, fs_, gf_, g_, gs_, af_ :: Octave -> Pitch
a_ o = (A, o)
as_ o = (As, o)
bf_ o = (Bf, o)
b_ o = (B, o)
c_ o = (C, o)
cs_ o = (Cs, o)
df_ o = (Df, o)
d_ o = (D, o)
ds_ o = (Ds, o)
ef_ o = (Ef, o)
e_ o = (E, o)
f_ o = (F, o)
fs_ o = (Fs, o)
gf_ o = (Gf, o)
g_ o = (G, o)
gs_ o = (Gs, o)
af_ o = (Af, o)

--tests
keyToNote :: Char -> Pitch
keyToNote 'a' = (a_ 4)
keyToNote 'b' = (b_ 4)
keyToNote 'c' = (c_ 4)

--charToMusic :: Char -> Music Pitch
charToMusic :: Char -> Maybe (Music Pitch)
charToMusic ch = case ch of
    'a' -> Just (a 4 1)
    'b' -> Just (b 4 1)
    'c' -> Just (c 4 1)
    'd' -> Just (d 4 1)
    'e' -> Just (e 4 1)
    'f' -> Just (f 4 1)
    'g' -> Just (g 4 1)
    _   -> Nothing

playingLB, playingRB :: Behavior Bool
playingLB = isOn lbp lbr
playingRB = isOn rbp rbr

--startA = when (playingLB)
--stopA = when (playingLB ==* rFalse)

--startB = when (playingRB)
--stopB = when (playingRB ==* rFalse)

startA = lbp
stopA = lbr
startB = rbp
stopB = rbr

--consistent tempo clocking with comparisons
r1 = animate $ showB (metronome 112) @@ (p2 50 50) $$ (showB time @@ (p2 50 150))

--moving metronome
amt1 = ((lift1 intToDouble) $ 10*(metronome 112)) -- this value should be observed at some point
r2 = animate $ el (p2 amt1 0) (p2 (25+amt1) 25) $$ (showB (metronome 112) @@ (p2 100 100))

--accumulating music objects
--r3 = animate $ showB (accum (rest 0) (rkey ==> (\m -> (:+: (charToMusic m))))) @@ (p2 50 50)
r3 = animate $ showB (accum (rest 0) (rkey ==> (\c -> let mMusic = (charToMusic c) in
                                                          case mMusic of
														       Nothing         -> id
														       (Just newMusic) -> (:+: newMusic)
                                                ))) @@ (p2 50 50)

aOn = el (p2 0 0) (p2 25 25)
aOff = el (p2 50 0) (p2 75 25)

bOn = el (p2 0 50) (p2 25 75)
bOff = el (p2 50 50) (p2 75 75)

-- Test keyUp : DOES NOT SEEM TO BE WORKING
r4 = animate $ switch aOn ( (keyUp -=> aOff) )

-- Keyboard-ish, tests startA and stopA
r5 = animate $ (switch aOn ( (startA -=> aOff) .|. (stopA -=> aOn))) $$ (switch bOn ( (startB -=> bOff) .|. (stopB -=> bOn)))
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
			
tloop tmp = pulse (1/(tmp/60)) --tmp represents beats per minute
wrap :: Int -> Double -> Behavior Bool
wrap b t = (((lift1 round) $ (hold 0 (tloop t))) %* (lift0 b)) ==* 0

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

keyToMusic ch = case ch of
	'a' -> a 4 1
	'b' -> b 4 1
	'c' -> c 4 1
	'd' -> d 4 1
	'e' -> e 4 1
	'f' -> f 4 1
	'g' -> g 4 1	
	
--consistent tempo clocking with comparisons
r1 = animate $ showB ((accum 0 ((tloop 112) -=> (+1))) %* 8) @@ (p2 50 50) $$ (showB time @@ (p2 50 150)) $$ (showB (hold 0 (tloop 112)) @@ (p2 50 100))

--a few little ticks with clocking
r2 = animate $ showB (choose (wrap 8 112) (lift0 "") (accum "" ((tloop 112) -=> (++ "|  ")))) @@ (p2 50 50)

--accumulating music objects
r3= animate $ showB (accum (rest 0) (rkey ==> (\m -> (:+: (keyToMusic m))))) @@ (p2 50 50)


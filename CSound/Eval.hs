{-# LANGUAGE Arrows #-}
module Eval where
import Euterpea 
import Parser
import Control.Arrow ((<<<), (>>>), arr)

type AudSF a b = SigFun AudRate a b

tableGen :: OscTable -> (Table, Double)
tableGen ot = let nam = ref ot
		  t0 = (args ot) !! 0
		  q = (args ot) !! 1
		  typ = (args ot) !! 2
		  hms = drop 3 (args ot) in
		case typ of
			10 -> (tableSinesN (round q) hms, t0) --GEN10 sin generator
			--fill in more types of table


--tests
o1 = OscTable { ref = "f1",
		args = [0,4096,10,1,0.5,0.33] }
		
t1 :: AudSF () Double
t1 = let (a1, a2) = tableGen o1 in 
	proc () -> do
	s <- osc a1 a2 -< 440
	outA -< s
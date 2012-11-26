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
testEvalIO file prs ev = do
	a <- readFile file
	let (ScoreSection b) = prs a
	let z = map ev b
	return $ map g z where
	g (tab, pha) = proc () -> do
		s <- osc tab pha -< 440
		outA -< s
		
{-t1 = let (a1, a2) = tableGen o1 in 
	proc () -> do
	s <- osc a1 a2 -< 440
	outA -< s-}
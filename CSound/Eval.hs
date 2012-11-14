{-# LANGUAGE Arrows #-}
module Eval where
import Euterpea 
import Parser
import Control.Arrow ((<<<), (>>>), arr)

tableGen :: OscTable -> (Table, Double)
tableGen ot = let nam = ref ot
		  t0 = (args ot) !! 0
		  dur = (args ot) !! 1
		  typ = (args ot) !! 2
		  hms = drop 3 (args ot) in
		case typ of
			10 -> (tableSinesN t0 hms, dur) --GEN10 sin generator
			--fill in more types of table



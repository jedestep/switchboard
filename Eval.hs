{-# LANGUAGE Arrows #-}
module Eval where
import Euterpea
import Parser
import Control.Arrow ((<<<), (>>>), arr)

simpleClip :: SigFun AudRate () Double
simpleClip = oscFixed 880

outs :: AudioSample a => SigFun AudRate () a -> IO ()
outs f = outFile "test.wav" 3 f
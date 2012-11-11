{-# LANGUAGE Arrows #-}
module Eval where
import Euterpea 
import Parser
import Control.Arrow ((<<<), (>>>), arr)

instance Clock VarRate where
rate = id

type VarSF a b = SigFun VarRate a b

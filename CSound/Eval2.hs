{-# LANGUAGE Arrows #-}
module Eval2 where
import Euterpea 
import Parser
import Control.Arrow ((<<<), (>>>), arr, (<<^))
import Control.Monad
import System.IO.Unsafe
import Debug.Trace
import qualified Data.Map as Map

type GenSF = AudSF [Double] Double

data CSEnv a = CSEnv (EMap -> (EMap, a))

type AudSF a b = SigFun AudRate a b
type EMap = (Map.Map String GenSF)

--monad definition
getMapState :: CSEnv EMap
getMapState = CSEnv (\m -> (m, m))

putMapState :: EMap -> CSEnv ()
putMapState m = CSEnv $ const (m, ())

runEval :: EMap -> CSEnv a -> (EMap, a)
runEval m (CSEnv f) = f m

instance Monad (CSEnv) where
	return v = CSEnv (\m -> (m, v))
	e >>= f = CSEnv (\m0 -> let (m1, v) = runEval m0 e in
			runEval m1 (f v))

--monad help functions
search :: String -> CSEnv GenSF
search key = do
	a <- getMapState
	return $ a Map.! key
	
store :: String -> GenSF -> CSEnv ()
store key val = do
	a <- getMapState
	putMapState $ Map.insert key val a
	
--orchestra files

define :: OrchestraRow -> CSEnv ()
define (Definition s t a) = case t of
	"oscil" -> do 
			sf <- oscil $ (lit $ last a)
			store s sf
			where lit (Lit a) = round a
define (Out s) = do
	sf <- express s
	store "__OUT" sf

express vx = do 
	case vx of
	 (Lit l) -> search $ show l
	 (Var v) -> search $ v
	 --TODO define cases
	
oscil :: Integer -> CSEnv GenSF
oscil num = do 
	sf <- search $ 'f':(show num) --assumes structured oscillator names
	return $ oscil1 sf where
		--oscil1 :: AudSF a Double -> AudSF a Double
		oscil1 i = proc a0 -> do
			s <- i -< a0
			outA -< s

--score files
tableGen :: OscTable -> (Table, Double, String)
tableGen ot = let nam = ref ot
		  a = args ot
		  t0 = a !! 0
		  q = a !! 1
		  typ = a !! 2
		  hms = drop 3 a in
		case typ of
			10 -> (tableSinesN (round q) hms, t0, nam) --GEN10 sin generator
			--fill in more types of table
	
oscillators :: ScoreSection -> [(String, GenSF)]
oscillators (ScoreSection scs) = map g $ map tableGen scs where
	g (tab, pha, nam) = (nam, (osc tab pha) <<^ head)
	
--evaluation
eval :: Program -> CSEnv GenSF
eval prog = let score = scrs prog
		(OrchestraSection os) = orch prog in
	    do
		mapM (\(n, sf) -> store n sf) (oscillators score)
		mapM define (orows $ head os) --todo fix this filth
		search "__OUT"
		
evaluate :: Program -> (GenSF)
evaluate p = snd $ runEval (Map.empty) (eval p)

evalCsound file = unsafePerformIO $ do
	a <- readFile file
	return $ evaluate $ parseProgram a

--tests

t1 = let a = evalCsound "example1.csd.test" in
	proc () -> do
	s <- a -< [525]
	outA -< s

testEvalScoreIO :: FilePath -> (String -> ScoreSection) -> (OscTable -> (Table, Double)) -> [AudSF () Double]
testEvalScoreIO file prs ev = unsafePerformIO $ do
	a <- readFile file
	let (ScoreSection b) = prs a
	let z = map ev b
	return $ map g z where
	g (tab, pha) = proc () -> do
		s <- osc tab pha -< 440
		outA -< s
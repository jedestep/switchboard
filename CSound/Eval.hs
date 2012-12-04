{-# LANGUAGE Arrows, ExistentialQuantification #-}
module Eval where
import Euterpea 
import Parser
import Control.Arrow ((<<<), (>>>), arr)
import Control.Monad
import System.IO.Unsafe
import qualified Data.Map as Map

data GenSF = forall a. SigFunInput a => GenSF (AudSF a Double)

data EvalM a = EvalM (EMap -> (EMap, a))

type AudSF a b = SigFun AudRate a b
type EMap = (Map.Map String GenSF)

--typeclass definition
class SigFunInput a

instance SigFunInput Double
instance SigFunInput ()
instance (SigFunInput a, SigFunInput b) => SigFunInput (a, b)

--monad definition
getMapState :: EvalM EMap
getMapState = EvalM (\m -> (m, m))

putMapState :: EMap -> EvalM ()
putMapState m = EvalM $ const (m, ())

runEval :: EMap -> EvalM a -> (EMap, a)
runEval m (EvalM f) = f m

instance Monad EvalM where
	return v = EvalM (\m -> (m, v))
	e >>= f = EvalM (\m0 -> let (m1, v) = runEval m0 e in
			runEval m1 (f v))

--monad help functions
search :: String -> EvalM GenSF
search key = do
	a <- getMapState
	return $ a Map.! key
	
store :: String -> GenSF -> EvalM ()
store key val = do
	a <- getMapState
	putMapState $ Map.insert key val a
	
--orchestra files

define :: OrchestraRow -> EvalM ()
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
	
oscil :: Integer -> EvalM GenSF
oscil num = do 
	(GenSF sf) <- search $ 'f':(show num) --assumes structured oscillator names
	return $ GenSF $ oscil1 sf where
		oscil1 :: (SigFunInput c) => AudSF a Double -> AudSF (c,a) Double
		oscil1 i = proc (a0, a1) -> do
			s <- i -< a1
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
	
oscillators :: ScoreSection -> [(String, AudSF Double Double)]
oscillators (ScoreSection scs) = map g $ map tableGen scs where
	g (tab, pha, nam) = (nam, osc tab pha)
	
--evaluation
eval :: Program -> EvalM GenSF
eval prog = let score = scrs prog
		(OrchestraSection os) = orch prog in
	     do
		return $ map (\(n, sf) -> store n sf) (map (\(s, sf) -> (s, GenSF sf)) (oscillators score))
		return $ map (map define) (map orows os)
		search "__OUT"

evaluate :: Program -> GenSF
evaluate p = snd $ runEval (Map.empty) (eval p)

--tests
testEvaluateIO :: String -> GenSF
testEvaluateIO file = unsafePerformIO $ do
	a <- readFile file
	return $ evaluate $ parseProgram a

testEvalScoreIO :: FilePath -> (String -> ScoreSection) -> (OscTable -> (Table, Double)) -> [AudSF () Double]
testEvalScoreIO file prs ev = unsafePerformIO $ do
	a <- readFile file
	let (ScoreSection b) = prs a
	let z = map ev b
	return $ map g z where
	g (tab, pha) = proc () -> do
		s <- osc tab pha -< 440
		outA -< s
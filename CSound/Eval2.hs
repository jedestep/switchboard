{-# LANGUAGE Arrows #-}
module Eval2 where
import Euterpea 
import Parser
import Control.Arrow ((<<<), (>>>), arr, (<<^), returnA, (^<<))
import Control.Monad
import System.IO.Unsafe
import Debug.Trace
import Data.List
import qualified Data.Map as Map

type GenSF = AudSF [Double] Double

data CSEnv a = CSEnv (SFMap -> (SFMap, a))

type AudSF a b = SigFun AudRate a b
type SFMap = (Map.Map String GenSF)

data InputType = User | Internal | Constant

--monad definition
getMapState :: CSEnv SFMap
getMapState = CSEnv (\m -> (m, m))

putMapState :: SFMap -> CSEnv ()
putMapState m = CSEnv $ const (m, ())

runEval :: SFMap -> CSEnv a -> (SFMap, a)
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
			let args = map express a
			sf <- oscil $ args
			store s sf
			--where lit (Lit a) = round a
define (Out s) = do
	sf <- search $ express s
	store "__OUT" sf

express :: VExpression -> String
express vx = do 
	case vx of
	 (Lit l) -> show l
	 (Var v) -> v
	 --TODO define cases
	
oscil :: [String] -> CSEnv GenSF
oscil nums = if (length nums) /= 3 then error "The wrong number of arguments were given to oscil!" else
	do 
	let fn = show $ round $ (read $ last nums :: Double)
	sf <- search $ 'f':fn --assumes structured oscillator names
	return $ sf 

wire :: [OrchestraRow] -> OrchestraRow -> CSEnv GenSF
wire rs (Definition s t a) = do
	f <- search s
	let argn = init a
	case t of
	  "oscil" -> do
			let (arg0:arg1:_) = map (\a' -> case (itype a') of
					Constant -> return (arr (\_ -> [read a' :: Double]))
					User	-> do {f' <- search a'; return ((\c -> [c]) ^<< f')}
					Internal -> (do 
					  let (Just r') = find (\(Definition s' _ _) -> s' == a') rs
					  f' <- wire rs r'
					  return ((\c -> [c]) ^<< f'))) (map express argn)
			arg0' <- arg0
			arg1' <- arg1
			let m1 = (head ^<< arg1') <<^ (\c -> [c])
			return $ proc _ -> do
				  a0 <- arg0' -< []
				  s <- f -< a0
				  a1 <- m1 -< 0
				  outA -< s * (a1/10000)
wire rs (Out a) = do
	let (Just r') = find (\(Definition s' _ _) -> s' == (express a)) rs
	f' <- wire rs r'
	return f'

itype :: String -> InputType		
itype (x:_) = case x of
	'p' -> User
	'a' -> Internal
	'k' -> Internal
	'i' -> Internal
	'0' -> Constant --todo fix this filth also
	'1' -> Constant
	'2' -> Constant
	'3' -> Constant
	'4' -> Constant
	'5' -> Constant
	'6' -> Constant
	'7' -> Constant
	'8' -> Constant
	'9' -> Constant
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
			--todo fill in more types of table
	
oscillators :: ScoreSection -> [(String, GenSF)]
oscillators (ScoreSection scs) = map g $ map tableGen scs where
	g (tab, pha, nam) = (nam, (osc tab pha) <<^ head)
	
--evaluation
eval :: Program -> CSEnv GenSF
eval prog = let score = scrs prog
		(OrchestraSection os) = orch prog in
	    do
		mapM (\(n, sf) -> store n sf) (oscillators score)
		let rs = orows $ head os --todo fix this megafilth
		let out = last rs
		mapM define rs 
		wire rs out
		
		
evaluate :: Program -> [Double] -> GenSF
evaluate p a = snd $ runEval (loadArgs Map.empty "p4" (map arr (map (\b -> (\[] -> b)) a))) (eval p)

loadArgs m _ [] = m
loadArgs m v (x:xs) = loadArgs (Map.insert v x m) (incp v) xs where
	incp v1 = 'p':(show (1 + (read $ tail v1 :: Int)))

evalCsound file args = blank >>> (unsafePerformIO $ do
	a <- readFile file
	return $ evaluate (parseProgram a) args)

blank :: AudSF () [Double]	
blank = arr (\() -> [])

--tests

testEvalScoreIO :: FilePath -> (String -> ScoreSection) -> (OscTable -> (Table, Double)) -> [AudSF () Double]
testEvalScoreIO file prs ev = unsafePerformIO $ do
	a <- readFile file
	let (ScoreSection b) = prs a
	let z = map ev b
	return $ map g z where
	g (tab, pha) = proc () -> do
		s <- osc tab pha -< 440
		outA -< s
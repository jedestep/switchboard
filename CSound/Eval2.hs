{-# LANGUAGE Arrows #-}
module Eval2 where
import Euterpea 
import qualified Euterpea.IO.Audio.CSound as CS
import Parser
import Control.Arrow 
import Control.Monad
import System.IO.Unsafe
import Debug.Trace
import Data.List
import Data.Maybe
import qualified Data.Map as Map

data ValSF = K Double | GenSF (AudSF [Double] Double)
data CSEnv a = CSEnv (SFMap -> (SFMap, a))

type AudSF a b = SigFun AudRate a b
type SFMap = (Map.Map String ValSF)

data InputType = User | Internal | Constant

--monad definition
getMapState :: CSEnv SFMap
getMapState = CSEnv (\m -> (m, m))

putMapState :: SFMap -> CSEnv ()
putMapState m = CSEnv $ const (m, ())

runEval :: SFMap -> CSEnv a -> (SFMap, a)
runEval m (CSEnv f) = f m

instance Monad CSEnv where
	return v = CSEnv (\m -> (m, v))
	e >>= f = CSEnv (\m0 -> let (m1, v) = runEval m0 e in
			runEval m1 (f v))
			
--Num instance
instance Num ValSF where
	(GenSF a) + (GenSF b) = GenSF $ proc _ -> do
		a' <- a -< []
		b' <- b -< []
		outA -< a'+b'
	(K a) + (K b) = K $ a+b
	(GenSF a) + (K b) = (GenSF a) + (GenSF $ arr $ const b)
	(K a) + (GenSF b) = (GenSF $ arr $ const a) + (GenSF b)
	(GenSF a) * (GenSF b) = GenSF $ proc _ -> do
		a' <- a -< []
		b' <- b -< []
		outA -< a'*b'
	(K a) * (K b) = K $ a*b
	(GenSF a) * (K b) = (GenSF a) * (GenSF $ arr $ const b)
	(K a) * (GenSF b) = (GenSF $ arr $ const a) * (GenSF b)
	abs = error "abs is undefined"
	signum = error "signum is undefined"
	fromInteger = error "fromInteger is undefined"

--monad help functions
search :: String -> CSEnv ValSF
search key = do
	a <- getMapState
	return $ a Map.! key
	
isDefined :: String -> CSEnv Bool
isDefined key = do
	a <- getMapState
	return $ case Map.lookup key a of 
		  Just _  -> True
		  Nothing -> False
	
store :: String -> ValSF -> CSEnv ()
store key val = do
	a <- getMapState
	putMapState $ Map.insert key val a
	
--orchestra files

define :: OrchestraRow -> CSEnv ()
define (Definition s t a) = case t of
	"oscil" -> do 
			exp <- mapM express a
			sf <- oscilD $ exp
			store s sf

express :: VExpression -> CSEnv ValSF
express vx = case vx of
		(Lit l) -> return $ K l
		(Var l) -> search l
		(x Parser.:+: y) -> do {a <- express x; b <- express y; return $ a+b}
		(x :*: y) -> do {a <- express x; b <- express y; return $ a*b}
		
unexpress :: VExpression -> [String]
unexpress vx = case vx of
		(Lit l) -> [show l]
		(Var l) -> [l]
		(x Parser.:+: y) -> (unexpress x)++(unexpress y)
		(x :*: y) -> (unexpress x)++(unexpress y)
	
oscilD :: [ValSF] -> CSEnv ValSF
oscilD nums = if (length nums) /= 3 then error "The wrong number of arguments were given to oscil!" else
	do 
	let (K l) = last nums
	let fn = show $ round l
	sf <- search $ 'f':fn --assumes structured oscillator names
	return $ sf 

resolve :: [OrchestraRow] -> String -> CSEnv ValSF
resolve rs = (\a' -> case (itype a') of
		Constant -> return $ GenSF (arr (\_ -> read a' :: Double))
		User	-> do
		  (K f') <- search a' --ensure that f' is a K
		  return $ K f'
		Internal -> (do --the blocker issue comes up here theoretically
		  let (Just r') = find (\(Definition s' _ _) -> s' == a') rs
		  (GenSF f') <- wire rs r' --ensure that f' is a GenSF
		  return $ GenSF f'))
		  
wire :: [OrchestraRow] -> OrchestraRow -> CSEnv ValSF
wire rs r@(Definition s t a) = do
	b <- isDefined s
	if (not b) then define r else return ()
	(GenSF f) <- search s
	case t of
	  "oscil" -> do
		let args = map show (init a)
		let (arg0:arg1:_) = map (resolve rs) args
		arg0' <- arg0
		arg1' <- arg1
		let (m0:m1:_) = map (\a -> case a of
					(K x) -> arr (\_ -> x)
					(GenSF x) -> x) [arg0',arg1']
		let res = GenSF $ proc _ -> do
			  a0 <- m0 -< []
			  s <- f -< [a0]
			  a1 <- m1 -< []
			  outA -< s * (a1/10000)
		store s res
		return res
	 {- "linen" -> do
		  
		-}
wire rs (Out a) = do
	let r0 = unexpress a
	let r1 = map (\z -> find (\(Definition s' _ _) -> (s' == z)) rs) r0
	let r1' = map fromJust r1
	mapM (wire rs) r1'
	express a

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
	
oscillators :: ScoreSection -> [(String, ValSF)]
oscillators (ScoreSection scs) = map g $ map tableGen scs where
	g (tab, pha, nam) = (nam, GenSF (head ^>> (osc tab pha)))
	
--evaluation
eval :: Program -> CSEnv ValSF
eval prog = let score = scrs prog
		(OrchestraSection os) = orch prog in
	    do
		mapM (\(n, sf) -> store n sf) (oscillators score)
		let rs = orows $ head os --todo fix this megafilth
		let out = last rs
		wire rs out
		
		
evaluate :: Program -> [Double] -> ValSF
evaluate p a = snd $ runEval (loadArgs Map.empty "p4" (map K a)) (eval p)

loadArgs m _ [] = m
loadArgs m v (x:xs) = loadArgs (Map.insert v x m) (incp v) xs where
	incp v1 = 'p':(show (1 + (read $ tail v1 :: Int)))

evalCsound file args = let (GenSF sf) = (unsafePerformIO $ do
				a <- readFile file
				return $ evaluate (parseProgram a) args) in
			blank >>> sf

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
{-# LANGUAGE Arrows #-}
module Switchboard.CSound.Eval where
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
{-
*** CSound to Euterpea evaluation
Evaluating a parsed CSound AST wil produce an individual SigFun as the output; the output is formatted for use in contexts such as outFile. For example:

> let output = evalCsound "example1.csd" []
> :t output
output :: SigFun AudRate () Double

evalCsound accepts two arguments: the name of the file to be evaluated, and a list of doubles containing all user parameters. The parameters correspond to score file parameters in CSound (parameters beginning with 'p'). The interpreter labels them starting from p4. So typing 
	evalCsound "example1.csd" [440,10000]
would interpret example1.csd where the p4 argument is 440.0 and the p5 argument is 10000.0. 

*** CSound library functions
Notably, support for CSound functions outside of oscil is currently absent. This is due to a particular problem with arrow syntax and the arrow preprocessor. Take for example the following CSound program:

instr 1
k1  linen  p4,p5,p6,p7
k2  linen  k1,p5,p6,p7
a1  oscil  440,k2,1
out a1

Here, k1 would be evaluated to be of type SigFun AudRate Double Double, using the linen function in Euterpea.IO.Audio.CSound. Since all of its first three arguments can be guaranteed to be constant (they are p-variables), we can simply dereference p4, p5, and p6 from the environment, get their values, and feed them into linen. We can then feed p7 into the resulting arrow and store this arrowized value into the environment as k1. 
How do we construct k2? It seems like we can do the same thing: simply get the values of k1, p5, and p6 and feed them into linen. However, the value of k1 has been made reactive by feeding it through linen once; it can no longer be used as an argument to linen. Thus it becomes clear that interpreting this program is actually fairly challenging.
There are two possible paths to tackle this issue. The far simpler one (which may also end up producing a more desirable result) would be to compile the input CSound file instead of interpreting it. Given the infrastructure in place, it would be a significant but not extremely difficult change to make to have this code output a source file instead of a signal function. This also has the added benefit of allowing Euterpea users to edit the result of CSound compilation easily. In order to do this, it would make sense to add an additional state to the CSEnv monad representing the source file, which would be built up with individual lines as lines of CSound are read. A monad transformer could also be used to incorporate the Text.PrettyPrint module.
The other path would be to eschew arrow preprocessor syntax and manually do the arrow computations necessary. While it would likely be possible to fudge out the required arrow computations, it would require changing the way the state is stored to include a list of previously evaluated variables with their values, which would then be available to the program as it evaluates other lines. This seems like the far larger amount of extra work.
-}

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
instance Num ValSF where --allow + and *; whenever we try to add reactive and non-reactive values, scale up to reactive
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
define (Definition varName fType args) = case fType of
	"oscil" -> do 
			exp <- mapM express args
			sigFun <- oscilD $ exp
			store varName sigFun

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
	sigFun <- search $ 'f':fn --assumes structured oscillator names
	return $ sigFun 

resolve :: [OrchestraRow] -> String -> CSEnv ValSF
resolve rowList name = case (itype name) of
		Constant -> return $ GenSF (arr (\_ -> read name :: Double))
		User	-> do
		  (K val) <- search name --ensure that f' is a K
		  return $ K val
		Internal -> do --the blocker issue comes up here theoretically
		  let (Just match) = find (\(Definition name1 _ _) -> name1 == name) rowList
		  (GenSF val) <- wire rowList match --ensure that f' is a GenSF
		  return $ GenSF val
		  
wire :: [OrchestraRow] -> OrchestraRow -> CSEnv ValSF
wire rowList row@(Definition varName fType args) = do
	def <- isDefined varName
	if (not def) then define row else return ()
	(GenSF val) <- search varName
	case fType of
	  "oscil" -> do
		let argv = map show (init args)
		let (arg0:arg1:_) = map (resolve rowList) argv
		arg0' <- arg0
		arg1' <- arg1
		let (m0:m1:_) = map (\a -> case a of
					(K x) -> arr (\_ -> x)
					(GenSF x) -> x) [arg0',arg1']
		let result = GenSF $ proc _ -> do
			  a0 <- m0 -< []
			  sigFun <- val -< [a0]
			  a1 <- m1 -< []
			  outA -< sigFun * (a1/10000)
		store varName result
		return result
	 {- "linen" -> do
		  
		-}
wire rowList (Out vx) = do
	let vxVars = unexpress vx
	let var = map fromJust (map (\z -> find (\(Definition name _ _) -> (name == z)) rowList) vxVars)
	mapM (wire rowList) var
	express vx

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
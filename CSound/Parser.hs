module Parser where
import Lexer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos (newPos)
import Data.Either

--Abstract syntax tree
data Program = Program OptionsSection OrchestraSection ScoreSection
data Tag = OpenTag String | CloseTag String -- <tagname>
data Variable = LocalVar String | GlobalVar String -- </tagname>
data OrchestraSection = OrchestraSection [OrchestraRow] OrchOut
data OrchestraRow = 
	InstrumentLabel Int | -- instr [instrID]
	Definition Variable DefType Args | -- [a,k,i] [osctype] [args]
	Equality Variable VExpression -- [setupvar] = [val]
data OrchOut = Out VExpression -- out [instrname]
data ScoreSection = ScoreSection [OscTable] deriving Show
data OscTable = OscTable {ref::String, args::[Double]} deriving Show -- f1 0 4096 10 1

data VExpression = 
	RawVar Variable |
	RawInt Int |
	VExpression :+: VExpression |
	VExpression :-: VExpression |
	VExpression :*: VExpression |
	VExpression :/: VExpression 
data OptionsSection = 
	Flags |
	OutFile String 
data Flag = Flag Char
	
type Flags = [Flag]
type Args = [VExpression]
type DefType = String	

--convenience
type TokenParser a = GenParser Token () a

--parsing
program :: TokenParser Program
program = do
	openTag "CsoundSynthesizer"
	a <- optBlock
	b <- orchBlock
	c <- scoreBlock
	closeTag "CsoundSynthesizer"
	return $ Program a b c
	
orchBlock :: TokenParser OrchestraSection
orchBlock = orchBlock
	
optBlock :: TokenParser OptionsSection
optBlock = optBlock

scoreBlock :: TokenParser ScoreSection
scoreBlock = do
	openTag "CsScore"
	a <- osctab
	term
	closeTag "CsScore"
	return $ ScoreSection a 

osctab :: TokenParser [OscTable]
osctab = manyTill osctab1 (try $ isToken (genName "e"))

osctab1 :: TokenParser OscTable
osctab1 = do
	(Name a) <- nam
	b <- manyTill nam term
	let b' = map numToDouble b
	return $ OscTable a b'

--insttab :: TokenParser [InstTable]
--insttab = insttab

openTag :: String -> TokenParser Tag
openTag tn = do
	lt
	(Name a) <- nam
	gt
	term
	case a of
	 _ | a==tn -> return $ OpenTag tn
	 _ -> error $ "unexpected tag name " ++ a
	 
closeTag :: String -> TokenParser Tag
closeTag tn = do
	lt
	divis
	(Name a) <- nam
	gt
	term
	case a of
	 _ | a==tn -> return $ CloseTag tn
	 _ -> error $ "unexpected tag name " ++ a

--utility
isToken :: (Token -> Bool) -> TokenParser Token
isToken f = token show (\t -> newPos "" 0 0) (\t -> if f t then Just t else Nothing)
		
operator (Punct x) = case x of
						"+"		-> plus
						"-"		-> minus
						"*"		-> mult
						"/"		-> divis
						">"		-> gt
						"<"		-> lt

genName :: String -> (Token -> Bool)
genName nm = (\x -> case x of
			Name z | nm == z -> True
			_	-> False)						
isFor x = case x of
		Name "for"	-> True
		_	-> False
isIn x = case x of
		Name "in"	-> True
		_	-> False
isColon x = case x of
		Punct ":"	-> True
		_	-> False
isName x = case x of
		Name x	-> True
		_	-> False
isEquals x = case x of
		Punct "="	-> True
		_		-> False
isOpenParen x = case x of
		Punct "("	-> True
		_		-> False
isCloseParen x = case x of
		Punct ")"	-> True
		_		-> False
isComma x = case x of
		Punct ","	-> True
		_		-> False
isPlus x = case x of
		Punct "+"	-> True
		_		-> False
isMinus x = case x of
		Punct "-"	-> True
		_		-> False
isMult x = case x of
		Punct "*"	-> True
		_		-> False
isDiv x = case x of
		Punct "/"	-> True
		_		-> False
isNumber x = case x of
		Number x 	-> True
		_	 	-> False
isTerm x = case x of
		Terminate 	-> True
		_		-> False
isIf x = case x of
		Name "if"	-> True
		_		-> False
isElse x = case x of
		Name "else"	-> True
		_		-> False
isGt x = case x of
		Punct ">"	-> True
		_		-> False
isLt x = case x of
		Punct "<"	-> True
		_		-> False

nam = isToken isName
num = isToken isNumber
eql = isToken isEquals
closeParen = isToken isCloseParen
openParen = isToken isOpenParen

plus = isToken isPlus
minus = isToken isMinus
mult = isToken isMult
divis = isToken isDiv
term = isToken isTerm
comma = isToken isComma
colon = isToken isColon
for = isToken isFor
forIn = isToken isIn
ifs = isToken isIf
els = isToken isElse
gt = isToken isGt
lt = isToken isLt

strToInt :: String -> Int
strToInt s = read s

numToDouble :: Token -> Double
numToDouble (Name x) = (fromIntegral . toInteger . strToInt) x

--testing
parseProgram x = parse program "?" (getTokenProgram x)
parseScoreBlock x = parse scoreBlock "?" (getTokenProgram x)

testIO file prs = do
	a <- readFile file
	return $ prs a
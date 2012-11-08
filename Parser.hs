module Parser where
import Lexer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos (newPos)

--Abstract syntax tree
data Program = Program OptionsSection OrchestraSection ScoreSection
data Tag = OpenTag String | CloseTag String -- <tagname>
data Variable = LocalVar String | GlobalVar String -- </tagname>
data OrchestraSection = 
	InstrumentLabel Int | -- instr [instrID]
	Definition Variable DefType Args | -- [a,k,i] [osctype] [args]
	Equality Variable VExpression | -- [setupvar] = [val]
	Out VExpression | -- out [instrname]
	EndIn -- endin
data ScoreSection = ScoreSection [OscTable] [InstTable]
data OscTable = OscTable String [Int]  -- f1 0 4096 10 1
data InstTable = InstTable String [Int] -- i2 0 1 2000 4000 8000

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
	a <- osctab
	b <- insttab
	isToken (genName "e")	
	return $ ScoreSection a b

osctab :: TokenParser [OscTable]
osctab = osctab

insttab :: TokenParser [InstTable]
insttab = insttab

openTag :: String -> TokenParser Tag
openTag tn = do
	gt
	(Name a) <- nam
	lt
	case a of
	 tn -> return $ OpenTag tn
	 
closeTag :: String -> TokenParser Tag
closeTag tn = do
	gt
	divis
	(Name a) <- nam
	lt
	case a of
	 tn -> return $ OpenTag tn

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
			Name nm -> True
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

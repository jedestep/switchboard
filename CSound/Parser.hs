module Parser where
import Lexer
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Parsec.Pos (newPos)
import Data.Either

--Abstract syntax tree
data Program = Program {orch::OrchestraSection, scrs::ScoreSection} deriving Show
data Tag = OpenTag String | CloseTag String -- <tagname>
data OrchestraSection = OrchestraSection [InstBlock] deriving Show
data InstBlock = InstBlock {ilabel::Int, orows::[OrchestraRow]} deriving Show
data OrchestraRow = 
	Definition String DefType Args | -- [a,k,i] [osctype] [args]
	Equality String VExpression | -- [setupvar] = [val]
	Out VExpression deriving Show -- out [instrname]
data ScoreSection = ScoreSection [OscTable] deriving Show
data OscTable = OscTable {ref::String, args::[Double]} deriving Show -- f1 0 4096 10 1

data VExpression = 
	Var String |
	Lit Double |
	VExpression :+: VExpression |
	VExpression :-: VExpression |
	VExpression :*: VExpression |
	VExpression :/: VExpression deriving Show
	
type Args = [VExpression]
type DefType = String	

--convenience
type TokenParser a = GenParser Token () a

--parsing
program :: TokenParser Program
program = do
	openTag "CsoundSynthesizer"
	a <- orchBlock
	b <- scoreBlock
	closeTag "CsoundSynthesizer"
	return $ Program a b
	
orchBlock :: TokenParser OrchestraSection
orchBlock = do
	openTag "CsInstruments"
	a <- many1 insttab
	closeTag "CsInstruments"
	optional term
	return $ OrchestraSection a
	
insttab :: TokenParser InstBlock
insttab = do
	a <- instlabel
	term
	b <- orchrows
	term
	return $ InstBlock a b
	
instlabel :: TokenParser Int
instlabel = do
	isToken $ genName "instr"
	(Number a) <- num
	return (strToInt a)
	
orchrows :: TokenParser [OrchestraRow]
orchrows = do
	try ( do 
		a <- orchrow
		term
		isToken (genName "endin") 
		return [a]
	    )
	    <|> do
		a <- orchrow
		term
		b <- orchrows
		return $ a:b
	    
orchrow :: TokenParser OrchestraRow
orchrow = do
	(Name a) <- nam
	if a=="out" 
	 then (do{b <- vexp; return $ Out b;}) 
	 else
	  try ( do
		(Name funct) <- nam
		b <- sepBy vexp comma
		return $ Definition a funct b
	      )
	      <|>
	      try ( do
		eql
		b <- vexp
		return $ Equality a b
	      )
	      <?> (error "Invalid row in orchestra file: " ++ a)
	

exptable 	= 	[[inf (Punct "*") (\a b -> a :*: b) AssocLeft, inf (Punct "/") (:/:) AssocLeft],
				[inf (Punct "+") (:+:) AssocLeft, inf (Punct "-") (:-:) AssocLeft]]
			where
				inf s f assoc = Infix (operator s >> return f) assoc

vexp :: TokenParser VExpression
vexp = buildExpressionParser exptable xpr2

xpr2 = (wrappedExpr <|> bareVar <|> bareNum)

wrappedExpr :: TokenParser VExpression
wrappedExpr = do
		openParen
		a <- vexp
		closeParen
		return a

bareVar :: TokenParser VExpression
bareVar = do
	(Name a) <- nam
	return $ Var a

bareNum :: TokenParser VExpression
bareNum = do
	(Number a) <- num
	return $ Lit (intToDouble . strToInt $ a)

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
	b <- manyTill (nam <|> num) term
	let b' = map numToDouble b
	return $ OscTable a b'

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

intToDouble :: Int -> Double
intToDouble = fromIntegral . toInteger

numToDouble :: Token -> Double --be cautious
numToDouble (Name x) = (fromIntegral . toInteger . strToInt) x
numToDouble (Number x) = (fromIntegral . toInteger . strToInt) x

removeInstruments :: ScoreSection -> ScoreSection
removeInstruments (ScoreSection b) = ScoreSection $ filter (\a -> ((ref a) !! 0) == 'f') b

--testing
parseProgram x = case parse program "?" (getTokenProgram x) of
			Left x -> error $ "Encountered parse error: " ++ show x
			Right x -> x
parseScoreBlock x = case parse scoreBlock "?" (getTokenProgram x) of
			Left x -> error $ "Encountered parse error: " ++ show x
			Right x -> removeInstruments x
parseOrchBlock x = case parse orchBlock "?" (getTokenProgram x) of
			Left x -> error $ "Encountered parse error: " ++ show x
			Right x -> show x

testParseIO file prs = do
	a <- readFile file
	return $ prs a
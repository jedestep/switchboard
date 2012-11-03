module Lexer where
import Text.ParserCombinators.Parsec
import Data.Either

type Parse a = GenParser Char () a
type LexedLine = [Token]
type LexedProgram = [LexedLine]
data Token = Name String | Number String | Punct String | Terminate deriving (Eq, Show)

--an entire program
lexedProgram :: Parse LexedProgram
lexedProgram = sepEndBy (many wspace >> lexTokenList) (many1 space)
	--I distrust separating by space instead of newline but it seems to produce better results
			
--a single token
lexToken :: Parse Token
lexToken = do
		try (do 
			optional (many wspace)
			a <- many1 alphaNum
			return (Name a)
			)
	<|> 
		try (do
			optional (many wspace)
			a <- many1 digit
			return (Number a))
	<|> 	do
			optional (many wspace)
			a <- allowableChar
			return (Punct  [a])

--representative of a single line			
lexTokenList :: Parse LexedLine
lexTokenList = do
		a <- lexToken
		try (do
			optional (many wspace)
			b <- lexTokenList
			optional (many wspace)
			return (a:b)
		    )
		    <|> return ([a]++[Terminate])

--any punctuation character permitted in the syntax		    
allowableChar = char '+' <|> 
		char '-' <|> 
		char '*' <|> 
		char '/' <|> 
		char '=' <|>
		char ';' <|>
		char '(' <|>
		char ')' <|>
		char ',' <|>
		char '.' <|>
		char '<' <|>
		char '>' <|>
		char '/' <|>
		char '\"'
					
	
--instead of ignoring comments while lexing, we remove them afterwards
removeComments :: LexedLine -> LexedLine
removeComments (l:ls) = case l of
				Punct ";" -> []
				Punct ":" -> l:(removeComments ls)
				_	  -> l:(removeComments ls)
removeComments [] = []

--more general than space; doesn't accept carriage return
wspace = char ' ' <|> char '\t'
	
--testing and other beauty

--reads a program from a file and lexes it
getTokenProgram :: String -> IO (LexedProgram)
getTokenProgram a = do
	b <- readFile a
	return (map removeComments (head $ rights [parse lexedProgram "?" b]))
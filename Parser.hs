module Parser where
import Lexer
data Tag = OpenTag String | CloseTag String

type TokenParser a = GenParser Token () a
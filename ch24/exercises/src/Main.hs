module Main where

import           Control.Applicative     ((<|>))
import           Data.Maybe              (fromMaybe)
import           Data.Ratio              ((%))
import qualified Text.Parser.Combinators as PC
import qualified Text.Trifecta           as P

stop :: P.Parser a
stop = P.unexpected "stop"

one :: P.Parser Char
one = P.char '1'

oneEof :: P.Parser Char
oneEof = P.char '1' <* PC.eof

one' :: P.Parser a
one' = one >> stop

oneTwo :: P.Parser Char
oneTwo = one >> P.char '2'

oneTwoEof :: P.Parser Char
oneTwoEof = oneTwo <* PC.eof

oneTwo' :: P.Parser a
oneTwo' = oneTwo >> stop

parser123 :: P.Parser String
parser123 = P.string "123" <|> P.string "12" <|> P.string "1"

myString :: String -> P.Parser String
myString msg = traverse P.char msg

p123 :: String -> IO ()
p123 msg = print $ P.parseString parser123 mempty msg

testParse :: P.Parser Char -> IO ()
testParse p = print $ P.parseString p mempty "123"

testParse' :: Show a => P.Parser a -> String -> IO ()
testParse' p msg = print $ P.parseString p mempty msg

a :: P.Parser String
a = P.try $ P.string "let" <* (P.notFollowedBy P.alphaNum)

pNL :: String -> IO ()
pNL s = putStrLn $ '\n' : s

type NumberOrString = Either Integer String

a' :: String
a' = "blah"

b' :: String
b' = "123"

c' :: String
c' = "123blah789"

parseNos :: P.Parser NumberOrString
parseNos = (Left <$> P.integer) <|> (Right <$> P.some P.letter)

fraction :: P.Parser Rational
fraction = do
  numerator <- P.decimal
  _ <- P.char '/'
  denominator <- P.decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

myDecimal :: P.Parser Double
myDecimal = do
  mbSign <- P.optional (P.oneOf "+-")
  first <- P.decimal
  mbSecondStr <- P.optional (P.char '.' >> P.some P.digit)
  let second = maybe 0.0 (\x -> (read x) / ((10 ^) . length $ x)) mbSecondStr
      magnitude = (fromInteger first) + second
  return $
    case mbSign of
      Just '-' -> magnitude * (-1.0)
      _        -> magnitude

decimalOrFraction :: P.Parser (Either Rational Double)
decimalOrFraction = (P.try (Left <$> fraction)) <|> (Right <$> myDecimal)

main' :: IO ()
main' = do
  let p f i = P.parseString f mempty i
  print $ p (P.some P.letter) a'
  print $ p P.integer b'
  print $ p parseNos a'
  print $ p parseNos b'
  print $ p (P.many parseNos) c'
  print $ p (P.some parseNos) c'
  print $ p (P.some P.letter) ""
  print $ p (P.many P.letter) ""

main'' :: IO ()
main'' = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoEof with \"12\":"
  testParse' oneTwoEof "12"
  testParse' (myString "abc") "abc"
  testParse' (P.string "abc") "abc"
  testParse' (myString "abc") "ab"
  testParse' (P.string "abc") "ab"
  p123 "123"
  p123 "12"
  p123 "1"
  print $ P.parseString (P.integer <* P.eof) mempty "123"
  print $ P.parseString (P.integer <* P.eof) mempty "123abc"

-- TODO
-- understand this
-- get :: Monad m => StateT s m s
-- put :: Monad m => s -> StateT s m ()
-- runStateT :: StateT s m a -> s -> m (a, s)
-- Prelude> runStateT (put 8) 7
-- ((),8)
-- Prelude> runStateT get 8
-- (8,8)
-- Prelude> runStateT (put 1 >> get) 8
-- (1,1)
-- Prelude> (runStateT $ put 1 >> get) 0
-- (1,1)
-- Prelude> (runStateT $ put 2 >> get) 10021490234890
-- Chapter Exercises
data StrOrNumb
  = Str String
  | Numb Integer
  deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [StrOrNumb]

type Metadata = [StrOrNumb]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Ord, Show)

parseSemVer :: P.Parser SemVer
parseSemVer = do
  major <- P.decimal
  _ <- P.char '.'
  minor <- P.decimal
  _ <- P.char '.'
  patch <- P.decimal
  release <- P.optional $ P.try (P.char '-' >> (ps strOrNumb []))
  metadata <- P.optional $ P.try $ P.char '+' >> ps strOrNumb []
  _ <- P.eof
  return $
    SemVer major minor patch (fromMaybe [] release) (fromMaybe [] metadata)

strOrNumb :: P.Parser StrOrNumb
strOrNumb =
  ((Numb <$> (P.try P.integer <* P.notFollowedBy (P.noneOf "-+."))) <|>
   (Str <$> P.some (P.noneOf "-+."))) :: P.Parser StrOrNumb

ps :: P.Parser a -> [a] -> P.Parser [a]
ps pa xs' = do
  x <- pa
  dot <- P.optional (P.try $ P.char '.')
  maybe (return $ xs' ++ [x]) (const (ps pa (xs' ++ [x]))) dot

instance Eq a => Eq (P.Result a) where
  P.Success x == P.Success y = x == y
  P.Failure _ == P.Failure _ = True
  _ == _ = False

-- TODO: Chapter exercises
main :: IO ()
main = do
  let psv = P.parseString parseSemVer mempty
  print $ psv "2.1.1" == P.Success (SemVer 2 1 1 [] [])
  print $
    psv "1.0.0-x.7.z.92" ==
    P.Success (SemVer 1 0 0 [Str "x", Numb 7, Str "z", Numb 92] [])
  print $
    psv "1.0.0-gamma+002" == P.Success (SemVer 1 0 0 [Str "gamma"] [Numb 2])
  print $
    psv "1.0.0-beta+oof.sha.41af286" ==
    P.Success (SemVer 1 0 0 [Str "beta"] [Str "oof", Str "sha", Str "41af286"])
-- *Main> P.parseString (ps strOrNumb []) mempty "beta"
-- Success [Str "beta"]
-- *Main> P.parseString (ps strOrNumb []) mempty "beta.taeu"
-- Success [Str "beta",Str "taeu"]
-- *Main> P.parseString (ps strOrNumb []) mempty "beta.taeu.123"
-- Success [Str "beta",Str "taeu",Numb 123]
-- *Main> P.parseString (ps strOrNumb []) mempty "beta.taeu.123ab"
-- Failure (ErrInfo {_errDoc = (interactive):1:14: error: expected: digit
-- beta.taeu.123ab<EOF>
--              ^       , _errDeltas = [Columns 13 13]})

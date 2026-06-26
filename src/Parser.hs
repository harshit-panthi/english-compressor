{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Text as T
import Data.Text (Text)
  
import Data.Char (toLower, isLower, isDigit, isAscii, toUpper)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)

type Parser = Parsec Void Text

parseEngAl :: Parser [EngAl]
parseEngAl = many (choice
  [ try pPeriodSpaceCap
  , try pPeriodNewlineCap
  , try pTrigram
  , try pDigram
  , try pCapital
  , pAlphanum
  , pUnicode
  ]) <* eof

pPeriodSpaceCap :: Parser EngAl
pPeriodSpaceCap = do
  _ <- char '.'
  _ <- char ' '
  c <- upperChar
  rest <- getInput
  setInput (T.cons (toLower c) rest)
  return (Trigram PeriodSpaceCap)

pPeriodNewlineCap :: Parser EngAl
pPeriodNewlineCap = do
  _ <- char '.'
  _ <- char '\n'
  c <- upperChar
  rest <- getInput
  setInput (T.cons (toLower c) rest)
  return (Trigram PeriodNewlineCap)

pTrigram :: Parser EngAl
pTrigram = Trigram <$> choice
  [ THE <$ istr "the"
  , AND <$ istr "and"
  , ENT <$ istr "ent"
  , ING <$ istr "ing"
  , FOR <$ istr "for"
  , NDE <$ istr "nde"
  , HAS <$ istr "has"
  , NCE <$ istr "nce"
  , TIS <$ istr "tis"
  , OFT <$ istr "oft"
  , STH <$ istr "sth"
  , MEN <$ istr "men"
  ]

pDigram :: Parser EngAl
pDigram = Digram <$> choice
  [ CommaSpace <$ try (char ',' >> char ' ')
  , TH <$ istr "th"
  , HE <$ istr "he"
  , IN <$ istr "in"
  , EN <$ istr "en"
  , NT <$ istr "nt"
  , RE <$ istr "re"
  , ER <$ istr "er"
  , AN <$ istr "an"
  , TI <$ istr "ti"
  , ES <$ istr "es"
  , ON <$ istr "on"
  , AT <$ istr "at"
  , SE <$ istr "se"
  , ND <$ istr "nd"
  , OR <$ istr "or"
  , AR <$ istr "ar"
  , AL <$ istr "al"
  , TE <$ istr "te"
  , CO <$ istr "co"
  , DE <$ istr "de"
  , TO <$ istr "to"
  , RA <$ istr "ra"
  , ET <$ istr "et"
  , ED <$ istr "ed"
  , IT <$ istr "it"
  , SA <$ istr "sa"
  , EM <$ istr "em"
  , RO <$ istr "ro"
  , LY <$ istr "ly"
  ]

pCapital :: Parser EngAl
pCapital = do
  c <- upperChar
  rest <- getInput
  setInput (T.cons (toLower c) rest)
  return Capital

pAlphanum :: Parser EngAl
pAlphanum = Alphanum <$> satisfy (\c -> c `elem` ([' '..'~'] ++ ['\n', '\t']))

pUnicode :: Parser EngAl
pUnicode = Unicode <$> satisfy (not . \c -> c `elem` ([' '..'~'] ++ ['\n', '\t']))

istr :: Text -> Parser ()
istr = void . string -- mapM_ (\c -> satisfy (\x -> toLower x == c))

data EngAl where
  Alphanum :: Char -> EngAl
  Capital  :: EngAl
  Unicode  :: Char -> EngAl
  Escape   :: EngAl
  Digram   :: Di   -> EngAl
  Trigram  :: Tri  -> EngAl
  deriving (Show, Eq, Ord)

data Di where
  TH :: Di; HE :: Di; IN :: Di; EN :: Di; NT :: Di
  RE :: Di; ER :: Di; AN :: Di; TI :: Di; ES :: Di
  ON :: Di; AT :: Di; SE :: Di; ND :: Di; OR :: Di
  AR :: Di; AL :: Di; TE :: Di; CO :: Di; DE :: Di
  TO :: Di; RA :: Di; ET :: Di; ED :: Di; IT :: Di
  SA :: Di; EM :: Di; RO :: Di; LY :: Di
  CommaSpace :: Di
  deriving (Show, Eq, Ord)

data Tri where
  THE :: Tri; AND :: Tri; ENT :: Tri; ING :: Tri
  FOR :: Tri; NDE :: Tri; HAS :: Tri; NCE :: Tri
  TIS :: Tri; OFT :: Tri; STH :: Tri; MEN :: Tri
  PeriodSpaceCap   :: Tri
  PeriodNewlineCap :: Tri
  deriving (Show, Eq, Ord)

deriving instance Enum Di
deriving instance Bounded Di
deriving instance Enum Tri
deriving instance Bounded Tri

allTokens :: [EngAl]
allTokens = Escape
          : Capital
          : map Alphanum ([' '..'~'] ++ ['\n', '\t']) 
          ++ map Digram  [minBound..maxBound]
          ++ map Trigram [minBound..maxBound]

isUnicode :: EngAl -> Bool
isUnicode (Unicode _) = True
isUnicode _ = False

render (Alphanum c)  = T.singleton c
render (Unicode c)   = T.singleton c
render (Digram di)   = T.pack (diToStr di)
render (Trigram tri) = T.pack (triToStr tri)
render Capital       = error "cant render capital"
render Escape        = error "cant render escape sequence"

diToStr TH = "th"; diToStr HE = "he"; diToStr IN = "in"
diToStr EN = "en"; diToStr NT = "nt"; diToStr RE = "re"
diToStr ER = "er"; diToStr AN = "an"; diToStr TI = "ti"
diToStr ES = "es"; diToStr ON = "on"; diToStr AT = "at"
diToStr SE = "se"; diToStr ND = "nd"; diToStr OR = "or"
diToStr AR = "ar"; diToStr AL = "al"; diToStr TE = "te"
diToStr CO = "co"; diToStr DE = "de"; diToStr TO = "to"
diToStr RA = "ra"; diToStr ET = "et"; diToStr ED = "ed"
diToStr IT = "it"; diToStr SA = "sa"; diToStr EM = "em"
diToStr RO = "ro"; diToStr LY = "ly"; diToStr CommaSpace = ", "

triToStr THE = "the"; triToStr AND = "and"; triToStr ENT = "ent"
triToStr ING = "ing"; triToStr FOR = "for"; triToStr NDE = "nde"
triToStr HAS = "has"; triToStr NCE = "nce"; triToStr TIS = "tis"
triToStr OFT = "oft"; triToStr STH = "sth"; triToStr MEN = "men"
triToStr PeriodSpaceCap   = ". "
triToStr PeriodNewlineCap = ".\n"

capFirst :: Text -> Text
capFirst t = case T.uncons t of
  Nothing      -> t
  Just (c, cs) -> T.cons (toUpper c) cs

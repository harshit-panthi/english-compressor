{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Parser
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Bit
import qualified Data.Vector.Unboxed as U
import Data.Char (ord, chr)
import Data.Bits (testBit, shiftR)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = putStrLn "Hello, Haskell!"

writeBitVecToRawFile :: FilePath -> U.Vector Bit -> IO ()
writeBitVecToRawFile path bitVec = do
    let strictBytes :: BS.ByteString
        strictBytes = cloneToByteString bitVec
        
    let lazyBytes :: BL.ByteString
        lazyBytes = BL.fromStrict strictBytes
        
    BL.writeFile path lazyBytes

escapeCode :: Map EngAl (U.Vector Bit) -> U.Vector Bit
escapeCode m = m Map.! Escape

-- 21 bits cover all of Unicode (U+000000 to U+10FFFF)
charBits :: Char -> U.Vector Bit
charBits c = U.fromList [Bit (testBit (ord c) i) | i <- [20,19..0]]

encode :: Map EngAl (U.Vector Bit) -> [EngAl] -> U.Vector Bit
encode tbl = U.concat . map encodeOne
  where
    encodeOne (Unicode c) = escapeCode tbl <> charBits c
    encodeOne tok         = tbl Map.! tok

decode :: Huffman -> U.Vector Bit -> [EngAl]
decode root bs = reverse $ helper root 0 [] where
  n = U.length bs
  
  helper :: Huffman -> Int -> [EngAl] -> [EngAl]
  helper _ i acc | i == n + 1 = acc
                 | i >  n + 1 = error "corrupted"
  helper (Leaf Escape) i acc =
    let chunk = U.toList (U.slice i 21 bs)
        cp    = sum [2^j | (j, Bit True) <- zip [20,19..0] chunk]
    in helper root (i + 21) (Unicode (chr cp) : acc)
  helper (Leaf tok) i acc = helper root i (tok : acc)
  helper (Node l r) i acc =
    let Bit b = bs U.! i
    in helper (if b then r else l) (i + 1) acc

detokenize :: [EngAl] -> Text
detokenize [] = T.empty
detokenize (Capital : tok : rest) = (capFirst $ render tok) <> detokenize rest
detokenize (Trigram PeriodSpaceCap : tok : rest) = ". " <> (capFirst $ render tok) <> detokenize rest
detokenize (Trigram PeriodNewlineCap : tok : rest) = ".\n" <> (capFirst $ render tok) <> detokenize rest
detokenize (tok : rest) = render tok <> detokenize rest

data Huffman = Leaf EngAl | Node Huffman Huffman deriving (Show, Eq)
newtype Weighted a = Weighted (Integer, a) deriving (Show)
type PQueue a = Heap a

compFreq :: [EngAl] -> Map EngAl Integer
compFreq = Map.fromListWith (+) . map (, 1) .filter (not . isUnicode)

fromFreqs :: Map EngAl Integer -> PQueue (Weighted EngAl)
fromFreqs freqs =
  let base    = Map.fromList $ map (\tok -> (tok, 0)) allTokens
      seeded  = Map.toList $ Map.unionWith (+) freqs base
  in  Heap.fromList . map (\(tok, w) -> Weighted (w, tok)) $ seeded

insertl :: Huffman -> Maybe Huffman -> Huffman
insertl t Nothing = t
insertl (Leaf tok) (Just t) = Node (Leaf tok) (t)
insertl (Node l r) (Just t) = error "Nodes cant be inserted"

buildHuffman :: PQueue (Weighted EngAl) -> Maybe Huffman
buildHuffman pq = helper (liftToHuffman pq Heap.empty)
  where
    liftToHuffman :: PQueue (Weighted EngAl)
                  -> PQueue (Weighted Huffman)
                  -> PQueue (Weighted Huffman)
    liftToHuffman src dst = case Heap.uncons src of
      Nothing                        -> dst
      Just (Weighted (w, tok), rest) ->
        liftToHuffman rest (Heap.insert (Weighted (w, Leaf tok)) dst)

    helper :: PQueue (Weighted Huffman) -> Maybe Huffman
    helper pq
      | Heap.size pq <= 1 = do
          (Weighted (_, t), _) <- Heap.viewMin pq
          return t
      | otherwise = case Heap.uncons pq of
          Just (Weighted (w1, t1), pq') -> case Heap.uncons pq' of
            Just (Weighted (w2, t2), pq'') ->
              helper $ Heap.insert (Weighted (w1 + w2, Node t1 t2)) pq''
            Nothing -> error "unreachable"
          Nothing -> error "unreachable"

code :: Huffman -> Map EngAl (U.Vector Bit)
code t = go t (U.empty :: U.Vector Bit) Map.empty where
  go :: Huffman -> (U.Vector Bit) -> Map EngAl (U.Vector Bit) -> Map EngAl (U.Vector Bit)
  go (Leaf tok) c acc = Map.insert tok c acc
  go (Node l r) c acc = Map.union left right where
    left  = go l (c <> U.fromList [Bit False]) acc
    right = go r (c <> U.fromList [Bit True]) acc

instance Eq (Weighted a) where
  Weighted (w1,_) == Weighted (w2,_) = w1 == w2

instance Ord (Weighted a) where
  (Weighted (w1,_)) <= (Weighted (w2,_)) = w1 <= w2

-- trained on Harry Potter and the Philosopher's Stone (please dont sue me JK Rowling)
hTree :: Huffman
hTree = (Node (Node (Node (Node (Leaf (Alphanum 's')) (Node (Node (Leaf (Digram AR)) (Leaf (Digram HE))) (Node (Node (Leaf (Alphanum '-')) (Node (Leaf (Digram DE)) (Leaf (Alphanum ',')))) (Leaf (Alphanum 'k'))))) (Node (Node (Leaf (Alphanum 'w')) (Leaf (Alphanum 't'))) (Node (Node (Node (Node (Node (Node (Leaf (Alphanum 'z')) (Leaf (Digram EM))) (Leaf (Digram ND))) (Leaf (Digram CO))) (Leaf (Digram RO))) (Node (Node (Leaf (Digram EN)) (Leaf (Digram ET))) (Leaf (Digram TH)))) (Node (Node (Leaf (Trigram AND)) (Leaf (Trigram PeriodSpaceCap))) (Leaf (Digram CommaSpace)))))) (Node (Node (Node (Node (Leaf (Alphanum '"')) (Node (Node (Leaf (Digram LY)) (Leaf (Digram ES))) (Leaf (Digram IT)))) (Leaf (Alphanum 'u'))) (Node (Leaf (Alphanum 'd')) (Node (Node (Leaf (Digram RE)) (Node (Leaf (Digram SA)) (Node (Node (Leaf (Trigram ENT)) (Node (Leaf (Trigram PeriodNewlineCap)) (Node (Leaf (Alphanum ':')) (Node (Node (Leaf (Trigram OFT)) (Node (Node (Node (Leaf (Alphanum '5')) (Leaf (Alphanum '\t'))) (Leaf (Alphanum '4'))) (Leaf (Alphanum '1')))) (Leaf (Trigram HAS)))))) (Leaf (Trigram FOR))))) (Leaf (Alphanum 'b'))))) (Node (Node (Leaf (Alphanum '\n')) (Leaf (Alphanum 'h'))) (Node (Node (Node (Leaf (Digram AT)) (Leaf (Digram ER))) (Leaf (Alphanum 'p'))) (Node (Leaf (Alphanum 'c')) (Node (Leaf (Trigram ING)) (Leaf (Digram ON)))))))) (Node (Node (Node (Node (Node (Leaf (Trigram THE)) (Node (Leaf (Digram ED)) (Leaf (Alphanum 'v')))) (Leaf (Alphanum 'i'))) (Node (Leaf (Alphanum 'l')) (Leaf Capital))) (Node (Node (Node (Leaf (Alphanum 'g')) (Leaf (Alphanum 'f'))) (Leaf (Alphanum 'a'))) (Node (Node (Node (Leaf (Digram TO)) (Node (Node (Node (Leaf (Digram NT)) (Leaf (Alphanum 'j'))) (Leaf (Alphanum '?'))) (Leaf (Digram TE)))) (Node (Leaf (Digram IN)) (Leaf (Alphanum '\'')))) (Node (Leaf (Alphanum 'n')) (Node (Node (Node (Node (Leaf (Alphanum 'x')) (Leaf (Alphanum 'q'))) (Leaf (Digram RA))) (Leaf (Digram OR))) (Node (Leaf (Digram AN)) (Leaf (Digram SE)))))))) (Node (Node (Node (Leaf (Alphanum 'e')) (Node (Leaf (Alphanum 'm')) (Leaf (Alphanum 'r')))) (Node (Leaf (Alphanum 'o')) (Node (Node (Leaf (Alphanum '.')) (Node (Leaf (Digram AL)) (Node (Leaf (Digram TI)) (Node (Leaf (Alphanum '!')) (Node (Node (Leaf (Trigram NDE)) (Leaf (Trigram MEN))) (Node(Node (Node (Leaf (Alphanum '(')) (Leaf (Trigram NCE))) (Node (Node (Node (Node (Leaf (Alphanum '2')) (Leaf (Alphanum '7'))) (Node (Leaf (Alphanum '9')) (Node (Node (Leaf (Alphanum '\\')) (Leaf (Alphanum '6'))) (Leaf (Alphanum '*'))))) (Node (Leaf (Alphanum '3')) (Node (Node (Node (Leaf (Alphanum '~')) (Leaf (Alphanum '8'))) (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf Escape) (Leaf (Alphanum '}'))) (Leaf (Alphanum '|'))) (Leaf (Alphanum '{'))) (Leaf (Alphanum '`'))) (Leaf (Alphanum '_'))) (Leaf (Alphanum '^'))) (Leaf (Alphanum ']'))) (Leaf (Alphanum '['))) (Leaf (Alphanum 'Z'))) (Leaf (Alphanum 'Y'))) (Leaf (Alphanum 'X'))) (Leaf (Alphanum 'W'))) (Leaf (Alphanum 'V'))) (Leaf (Alphanum 'U'))) (Leaf (Alphanum 'T'))) (Leaf (Alphanum 'S'))) (Leaf (Alphanum 'R'))) (Leaf (Alphanum 'Q'))) (Leaf (Alphanum 'P'))) (Leaf (Alphanum 'O'))) (Leaf (Alphanum 'N'))) (Leaf (Alphanum 'M'))) (Leaf (Alphanum 'L'))) (Leaf (Alphanum 'K'))) (Leaf (Alphanum 'J'))) (Leaf (Alphanum 'I'))) (Leaf (Alphanum 'H'))) (Leaf (Alphanum 'G'))) (Leaf (Alphanum 'F'))) (Leaf (Alphanum 'E'))) (Leaf (Alphanum 'D'))) (Leaf (Alphanum 'C'))) (Leaf (Alphanum 'B'))) (Leaf (Alphanum 'A'))) (Leaf (Alphanum '@'))) (Leaf (Alphanum '>'))) (Leaf (Alphanum '='))) (Leaf (Alphanum '<'))) (Leaf (Alphanum '/'))) (Leaf (Alphanum '+'))) (Leaf (Alphanum '&'))) (Leaf (Alphanum '%'))) (Leaf (Alphanum '$'))) (Leaf (Alphanum '#'))) (Leaf (Trigram TIS))) (Leaf (Trigram STH)))) (Leaf (Alphanum '0'))))) (Leaf (Alphanum ')')))) (Leaf (Alphanum ';')))))))) (Leaf (Alphanum 'y'))))) (Leaf (Alphanum ' ')))))

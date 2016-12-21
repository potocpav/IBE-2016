{-# LANGUAGE ExistentialQuantification #-}

module Cipher where

import qualified Data.Char as Ch (ord,chr,toUpper)
import Data.List (findIndex, nub, elemIndex, permutations)
import Data.Maybe (fromJust)

ord :: Char -> Int
ord c = (Ch.ord $ Ch.toUpper c) - Ch.ord 'A'

chr :: Int -> Char
chr i = Ch.chr $ (i `mod` 26 + 26) `mod` 26 + Ch.ord 'A'

-- | passPad "BIOLOGY" = "BIOLGYACDEFHJKMNPQRSTUVWXZ"
passPad :: String -> String
passPad s = nub (s ++ ['A'..'Z'])

-- | All possible table sizes based on the input length
sizeRanges :: Int -> [(Int, Int)]
sizeRanges n = [(x, n `div` x) | x <- [2..n-1], n `rem` x == 0]

-- | A generic type containing any cipher
data AnyCipher = forall c. (Cipher c, Show c) => Any c
instance Show AnyCipher where
    show (Any c) = show c
instance Cipher AnyCipher where
    encode (Any c) = encode c
    decode (Any c) = decode c
instance Cipher a => Cipher [a] where
    encode cs s = foldr encode s cs
    decode cs s = foldr decode s (reverse cs)

-- NOTE: could have been a simple sum type because less generality was needed
-- after a refactor.
class Cipher c where
    encode :: c -> String -> String
    decode :: c -> String -> String
    space  :: [String] -- ^ Dictionary
           -> String   -- ^ Ciphertext
           -> [c]
    space _ _ = undefined

data Offset = Offset Int deriving (Show)
instance Cipher Offset where
    encode (Offset a) = map (\c -> chr $ ord c + a)
    decode (Offset a) = encode (Offset (-a))
    space _ _           = map Offset [0..25]

data Affine = Affine Int Int deriving (Show)
instance Cipher Affine where
    encode (Affine a b) = map (\c -> chr $ ord c * a + b)
    decode (Affine a b) = map (\c -> chr $ ainv a * (ord c  - b)) where
        ainv a = fromJust $ findIndex (\i -> (i * a) `mod` 26 == 1) [0..25]
    space _ _           = [Affine a b | a <- [1,3,5,7,9,11,15,17,19,21,23], b <- [0..25]]

data Substitution = Substitution String deriving (Show)
instance Cipher Substitution where
    encode (Substitution s) = map (\c -> s !! fromJust (elemIndex c ['A'..'Z']))
    decode (Substitution s) = map (\c -> ['A'..'Z'] !! fromJust (elemIndex c s))
    space s _               = map (Substitution . passPad) s

data Vigenere = Vigenere String deriving (Show)
instance Cipher Vigenere where
    encode (Vigenere s) = zipWith (\a b -> chr $ ord a + ord b) (concat $ repeat s)
    decode (Vigenere s) = zipWith (\a b -> chr $ ord b - ord a) (concat $ repeat s)
    space s _           = map Vigenere s

data Table = Table Int Int deriving (Show)
instance Cipher Table where
    encode (Table x y) = encode (PermTable x y [0..x-1])
    decode (Table x y) = encode (Table y x)
    space _ s          = map (uncurry Table) (sizeRanges $ length s)

data PermTable = PermTable Int Int [Int] deriving (Show)
instance Cipher PermTable where
    encode (PermTable x y p) = \s -> [s !! (j * x + i) | i <- p, j <- [0..y-1]]
    decode (PermTable y x p) = \s -> [s !! (j * x + i) | i <- [0..x-1], j <- p]
    space _ s                = [PermTable x y p | (x,y) <- sizeRanges (length s), x <= 7, p <- permutations [0..x-1]]

data DoubleTable = DoubleTable Int Int Int Int deriving (Show)
instance Cipher DoubleTable where
    encode (DoubleTable x1 y1 x2 y2) = encode (Table x2 y2) . encode (Table x1 y1)
    decode (DoubleTable x1 y1 x2 y2) = encode (DoubleTable y2 x2 y1 x1)
    space _ s                        = [DoubleTable x1 y1 x2 y2 | Table x1 y1 <- space [] s, Table x2 y2 <- space [] s]

module Lang where

import Data.Char
import Data.List (tails, foldl')
import Data.Serialize
import Control.Monad
import System.IO

import Data.Map.Strict (Map, insertWith, (!))
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B

data Lang = Lang { n :: Int, m :: Map String Double } deriving (Show)

instance Serialize Lang where
    put (Lang n m) = put (n, m)
    get = liftM (\(n,m) -> Lang n m) get


loadDict :: String -> IO [String]
loadDict f = (map strip . lines) <$> readFile f


strip :: String -> String
strip = filter (`elem` ['A'..'Z']) . map toUpper


llhood :: Lang -> String -> Double
llhood (Lang n m) s = let
    ngr = ngrams n s
    len = fromIntegral (length (strip s) - n + 1)
    in Map.foldlWithKey (\a k n -> a + n * log (m ! k)) 0 ngr / len


-- | very single-purpose convenience function to convert a raw file to a file
-- loadable by loadLang
genEnglish :: Int -> IO ()
genEnglish n = do
    l <- readFile' "data/englishText_0_10000" >>= return . newLang n
    saveLang ("eng"++show n++".lang") l


-- | Save to a file
saveLang :: String -> Lang -> IO ()
saveLang f l = B.writeFile f (encode l)


-- | Load from a file
loadLang :: String -> IO Lang
loadLang f = do
    b <- B.readFile f
    case decode b of
        Left err -> error err
        Right val -> return val


-- | Create a language from a representative (typically long) string
newLang :: Int -> String -> Lang
newLang n s = Lang n (ngramProb n s)


-- | Encoding-unaware function for reading a file
readFile' :: String -> IO String
readFile' f = do
    h <- openFile f ReadMode
    hSetEncoding h char8
    hGetContents h


-- | Map of probabilities of the last character being found after the init string
ngramProb :: Int -> String -> Map String Double
ngramProb n s = let
    ngr = ngrams n s
    mgr = ngrams (n-1) (init s)
    in Map.mapWithKey (\k n -> n / (mgr ! init k)) ngr


-- | Return the number of given n-grams
ngrams :: Int -> String -> Map String Double
ngrams n = ngrams' . initN n . tails . strip . stripTags where
    ngrams' = foldl' (\m s -> insertWith (+) (take n s) 1 m) Map.empty
    initN 0 = id
    initN n = init . initN (n-1)


stripTags :: String -> String
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs

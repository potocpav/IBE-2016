module Main where

import Data.Ord (comparing)
import Data.List (maximumBy, foldl')

import Lang
import Cipher
import Draw


ciphertexts :: [String]
ciphertexts = [ "PSSOEWWEHAEWTSMRXMRKEXXLIKMKERXMGJMKYVIMRXLIWXSGOVIZIRYIPMRIALEXHSCSYXLMRO"
              , "LWICOTLWUPFSKWKHWJUBTLWHUCJRQPGOMLUUFGCPGILUCTTWPRWRUPCOMLUFCJOLQZSUYMCPOWWTLCTLWICOEYQTWRQBBWJWPTBJUKTLWUTLWJOTCAWCFUUALWTYJPWRTLWZCGWOHCMA"
              , "GRWSQSCMTRCAGLERFAGPMWLKSQRAPQSLBBADYGLERFASTRFMPGRGAQGLRFACGEEAQRWSYNMQQGCJASLBQKMIGLEFSQFPGEFRLAXRRMRFACMSPBGLEQUFMMJWSQNPARRYKTUFRFAWMPQRYMTUMTJBBMQMRFAYNSQQABRFANGNASPMTLBSLBKMUIABRFARASUFAPQRPYGLERMMTRBMASUFMRFAPGKSEGLGLEWFSRRFAYWMTJBBMRMRFAKGDRFAYUMTJB"
              , "KHPFCBOADXOHYHTZUYAKPMZHFAEUQGZIDEVVLWLEGBEEZRHUWBPEHGZEJPSIWRSMRVETPXZVFGETSUWAOYOFLECFVRTOJEQEWAXQRNLTSQAAGTTOWAYTZAZNLESAKDMINWZLLHPAZQWRDFIQWNEEZNMGSFSEXEWXGVDEYFOFHRTSVGDYMAMJZOHMGHKEOFCGSUYFWAYOETSEKHPOCHDDSMJRWADUZLGUEDIALHPYUVNEYTCJDOLPSQLHPKKRJEMGHGZEETWPCEEIOFGVPDUEGWYMBQLHPPOAYECTSQHUEFVREIYFCBYRPMHSGRETSZLOWQHGZAETOCHEY"
              , "LYHTHFYTFMEOEEAEDFHATUYDDTIOITGIHTMEFRSEODEHARFMCSOISEDRIALXFOIBEICNAXMTTOLBUYSXETAYILLOSX"
              , "WORERKLTHACLETUHNAXNSNOENIDNHASOATSRSXTHKHOCAEPFNEMEONIIETAEYDGHOEENEUTTPIFXTDTNOYVSYOHDLMETTCXEUOBTILOOTELDHNEKNXYNRSMLAIMRDNBAHASAX"
              , "OGWAEEUOLTTUOVDDTHYITIOCNVAOEAEDTDOARTEYREGLEOLFUEESOELFWNNUTECUATADDTNNWHNYYPODBOEETYRRVBLYETFRRYKSMERIOEELEETOOYAYADSLHOWEEOBONHAIEONAODCTYLRECLAUNONOSRHLSUAARYEUTESOSUONRHHLOLTTAINLEUDUTIITHTTCHDLGYSCUPWWDWIIEUEOYNTDIYRGUAGTHUHETIIIOTTISOHAKEMNVSVKOHTSTOMINRHTETERIMDEBHRNSNANRCX"
              , "JFOJVFCHIPOOWFOSSBLBUUGFEHIUDPUISBSBUJPLONBVDSDPTPUHJWFUFDSGLPCHFXIUPVBIIVICXFVSNUJPUUFXOSEPINOUOFX"
              , "IRSKCIIGIRIDHTNHTHGCXSPTHXGCDSTPJWBSWIXEXXGCADTCTSGECJPICQHTIAWVUXQCHTWITTCGOPJTTVCHPXTDRBSAVTIIPSPKUCJDXRPWBSIPTA"
              , "IRCKKDMRTGLIVKQSQKIMHKSQQYQGSSDYSDKCQNMGKBYMCKCFLRLZBKKGGGRHCMBBBKDMMXXGBLOWRVQRQMBAMMGADOYNMMMKKRDMMBRRSMNGLTGDSGKVQJMQBHDDBYQSDBQTQNGRGKOXKJLGHSNDKMBBRAAVBCSCDGYYJMGIJNKODBXVDWSLGSCVSDLBMGQDRMKSOCSHSQGHQBHSOXDGBDRCHMKGAGDNRRDJSGGKNYMGNRKSLYCTX"
              ]


-- | Try out a list of ciphers and select the one that gives the best result
bruteForce ::  [(Lang, [AnyCipher])] -- ^ Possible cipher space. Multiple ciphers may be sequentially applied (outer list),
                                     --   and may have different langs.
           -> String                 -- ^ Ciphertext
           -> [AnyCipher]            -- ^ Sequence of ciphers that decodes the ciphertext
bruteForce css s = reverse . snd $ foldl' fold_f (s, []) css where
    best s' (l, cs) = maximumBy (comparing (\c -> llhood l $ decode c s')) cs
    fold_f (s', cs') cs = let c = best s' cs in (decode c s', Any c:cs')


-- | Nicely print a cipher and a plaintext (from a supplied ciphertext)
printRes :: [AnyCipher] -> String -> IO ()
printRes cs s = putStrLn $ show cs ++ ": \n" ++ decode cs s ++ "\n"


-- | Generate and save a frequency graph of the English language
genEnglishFreq :: IO ()
genEnglishFreq = do
    f <- readFile' "englishText_0_10000"
    freqGraph "english.png" (strip.stripTags $ f)


main = do
    l1 <- loadLang "data/eng1.lang" -- 1-gram freqs
    l2 <- loadLang "data/eng2.lang" -- 2-gram freqs
    -- Deciphering is very slow (a few minutes), due to a huge dictionary (200k words).
    dict <- loadDict "data/wordsEn.txt"

    let specific_ciphers = [ \c -> [(l2, map Any (space dict c :: [Offset]))]
                           , \c -> [(l2, map Any (space dict c :: [Affine]))]
                           , \c -> [(l2, map Any (space dict c :: [Substitution]))]
                           , \c -> [(l2, map Any (space dict c :: [Vigenere]))]
                           , \c -> [(l2, map Any (space dict c :: [Table]))]
                           , \c -> [(l2, map Any (space dict c :: [PermTable]))]
                           , \c -> [(l2, map Any (space dict c :: [DoubleTable]))]
                           , \c -> [(l1, map Any (space dict c :: [Offset])),       (l2, map Any (space dict c :: [Table]))]
                           , \c -> [(l1, map Any (space dict c :: [Offset])),       (l2, map Any (space dict c :: [DoubleTable]))]
                           , \c -> [(l1, map Any (space dict c :: [Substitution])), (l2, map Any (space dict c :: [PermTable]))]
                           ]

    -- Targeted deciphering; trying all the ciphers on all the ciphertexts (no need to guess the cipher)
    -- would require minor modifications to this line and the bruteForce function.
    let ciphers = zipWith (\a b -> bruteForce (a b) b) specific_ciphers ciphertexts
    mapM_ (uncurry printRes) (zip ciphers ciphertexts)

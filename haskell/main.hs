import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.Vector as V
import Data.Char (ord)
import System.Environment (getArgs)

data CompiledPattern = CompiledPattern BS.ByteString (V.Vector Int) (V.Vector Int) deriving Show
compute :: BS.ByteString -> CompiledPattern
compute p =
    let bc = fst . BS.foldl' (\(v, i) c -> (v V.// [(fromIntegral c, i)], i+1)) (V.replicate 256 (-1), 0) $ p in
    let (kmp, gs, _, _) = f (V.replicate (m+1) (-1) V.// [(m, m+1)]) (V.replicate (m+1) 0) m (m+1) in
    let (gs', _) = foldl (\(gs, j) i -> (if gs V.! i == 0 then gs V.// [(i,j)] else gs, if i==j then kmp V.! j else j)) (gs, kmp V.! 0) [0..m] in
    CompiledPattern p bc gs'
    where g kmp gs j i =
            if j <= m && (p `BS.index` (i-1) /= p `BS.index` (j-1)) then
                g kmp (if gs V.! j == 0 then gs V.// [(j, j-i)] else gs) (kmp V.! j) i
            else
                (gs, j)
          f kmp gs i j =
            if i > 0 then
                let (gs', j') = g kmp gs j i in
                f (kmp V.// [(i-1,j'-1)]) gs' (i-1) (j'-1)
            else
                (kmp, gs, i, j)
          m = BS.length p

search :: CompiledPattern -> BS.ByteString -> Int -> Int
search cp@(CompiledPattern p bc gs) t i =
    if i <= n-m then
        let j = f (m-1) in
        if j <= -1 then i
        else search cp t (i + max (gs V.! (j+1)) (j-(bc V.! (fromIntegral (t `BS.index` (i+j))))))
    else (-1)
    where m = BS.length p
          n = BS.length t
          f j = if j >= 0 && p `BS.index` j == t `BS.index` (i+j) then f (j-1) else j

packStr = BS.pack . map (fromIntegral . ord)

test :: IO ()
test =
    let tests = [("EXAMPLE", "HERE IS A SIMPLE EXAMPLE", 17),
                 ("EAAMPLEAMPLE", "HERE IS A SIMPLE EAAMPLEAMPLE", 17),
                 ("EXAMPLE", "HERE IS A SIMPLE EXAMPLA", -1),
                 ("ABCDABD", "ABC ABCDAB ABCDABCDABDE", 15),
                 ("EXAMPLS", "HERE IEXAMPLS", 6),
                 ("AMPNAM", "AAANAMPNAM", 4)
                ] in
    print . all testSingle $ tests
    where testSingle (p, t, r) = (search (compute . packStr $ p) (packStr t) 0) == r

checkAll :: BS.ByteString -> Int -> BS.ByteString -> IO ()
checkAll pat off str = if i > 0 then putStrLn (show i) >> checkAll pat (i+1) str else return ()
 where cpat = compute pat
       i = search cpat str off

main :: IO ()
main = do
    args <- getArgs
    case args of
      pat : []        -> pack <$> getContents   >>= checkAll (pack pat) 0
      pat : file : [] -> pack <$> readFile file >>= checkAll (pack pat) 0
      _               -> putStrLn "Invalid arguments"


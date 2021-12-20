{-# language PatternGuards #-}
{-# language ViewPatterns #-}
{-# language NoMonomorphismRestriction #-}
{-# language BlockArguments #-}
import Data.Function
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Time.Clock
import Control.Arrow
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Options.Applicative hiding (asum)
import System.IO
import System.Random
import System.Random.Shuffle

import KMNProgramming

data Method = Masum | Mrandom (Maybe Int) | Mb | Mnosort
 deriving (Read, Show, Eq, Ord)

readMethod = eitherReader $ \s -> case s of
    "nosort" -> Right Mnosort
    "asum" -> Right Masum
    "b" -> Right Mb
    (splitAt 6 -> ("random", s))
        | "" <- s -> Right $ Mrandom Nothing
        | [(i, "")] <- reads s -> Right $ Mrandom $ Just i
    _ -> Left $ "available sort methods: asum random randomSEED b nosort"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    join $ execParser opts
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Maximalize sum of the input matrix multiplied by tensor products of two vectors of +-1 elements"
     <> header "kmn-programming - specialized quadratic binary optimization")

    options :: Parser (IO ())
    options = 
        hsubparser
           ( command "sample" sampleCommand
          <> command "test" testCommand
          <> command "timerandom" randomCommand
           )
     <|> computeOptions

    sampleCommand = flip info (progDesc "print sample matrix") $ printTestMat
        <$> optional (strOption $ short 'o' <> long "output" <> metavar "FILE" <> help "output file" <> action "filenames")
        <*> argument auto (metavar "NAT")

    testCommand = flip info (progDesc "basic self-test") $ testTestMat
        <$> optional (argument auto $ metavar "NAT")

    randomCommand = flip info (progDesc "measure optimization time on random matrices") $ timeRandom
--        <$> optional (strOption $ short 'o' <> long "output" <> metavar "FILE" <> help "output file" <> action "filenames")
        <$> argument auto (metavar "NAT" <> help "biggest integer in random matrices")
        <*> argument auto (metavar "NAT" <> help "smallest width")
        <*> argument auto (metavar "NAT" <> help "biggest width")
        <*> argument auto (metavar "NAT" <> help "width step")
        <*> argument auto (metavar "NAT" <> help "smallest height")
        <*> argument auto (metavar "NAT" <> help "biggest height")
        <*> argument auto (metavar "NAT" <> help "height step")
        <*> argument auto (metavar "NAT" <> help "smallest level")
        <*> argument auto (metavar "NAT" <> help "biggest level")
        <*> argument auto (metavar "NAT" <> help "repeat computation")

    computeOptions = compute
        <$> switch (short 'd' <> long "delete" <> help "delete 0 rows and columns")
        <*> switch (short 't' <> long "transpose" <> help "transpose matrix if it has more rows than columns")
        <*> optional (option readMethod $ short 's' <> long "sort" <> metavar "SORTMETHOD" <> help "sort method - default is nosort" <> completeWith ["asum","random","b","nosort"])
        <*> switch (short 'm' <> long "multiply" <> help "multiply rows by +-1 to improve the first guess")
        <*> switch (short 'p' <> long "print" <> help "print matrix before optimization")
        <*> switch (short 's' <> long "silent" <> help "print just the result")
        <*> (fromMaybe 0 <$> optional (option auto $ short 'g' <> long "guess" <> metavar "NAT" <> help "guessed result - default is 0" <> completeWith ["0"]))
        <*> optional (strOption $ long "levelin" <> metavar "FILE" <> help "precomputed levels input file" <> action "filenames")
        <*> optional (strOption $ long "levelout" <> metavar "FILE" <> help "levels output file" <> action "filenames")
        <*> optional (option auto $ short 'l' <> long "level"  <> metavar "NAT" <> help "level - default is number of rows / 4")
        <*> (fromMaybe 0 <$> optional (option auto $ long "trace"  <> metavar "NAT" <> help "trace level - default is 0" <> completeWith ["0"]))
        <*> (fromMaybe 4 <$> optional (option auto $ short 'u' <> long "unroll" <> metavar "NAT" <> help "unroll cycles - default is 4" <> completeWith ["4"]))
        <*> (fromMaybe 8 <$> optional (option auto $ short 'a' <> long "align" <> metavar "NAT" <> help "code alignment - default is 8" <> completeWith ["8"]))
        <*> optional (strOption $ short 'o' <> long "output" <> metavar "FILE" <> help "output file" <> action "filenames")
        <*> (   {-Right . mkMatrix . filter (not . null) . concatMap splitSC <$ argument (readChars "matrix") (metavar "MATRIX") <*> many (argument (maybeReader readMat) (metavar "INT"))
            <|> -}Left  <$> argument str (metavar "FILE" <> action "filenames")
            )
        <*> optional (option auto $ long "timeout" <> help "timeout in seconds")
        <*> (fromMaybe (Partial (Just 1) 0) <$> optional (option auto $ long "partial" <> help "do partial computation"))

readChars c = maybeReader f where
    f c' | c == c' = Just ()
    f _ = Nothing

readMat s@(_:_) = Just s
readMat _ = Nothing

mkMatrix s = case span (/= ",") s of
    (i, ",": s) -> i: mkMatrix s
    (i, []) -> [i]

splitSC s = case span (/= ',') s of
    (i, ',': s) -> i: ",": splitSC s
    (i, []) -> [i]

checkPred err p | p = return ()
checkPred err _ = error err

data Partial
    = Partial (Maybe Int) Int
    deriving (Eq, Ord, Show)

instance Read Partial where
    readsPrec _ ('*':'/':'2':'^': s)
        | [(j,s)] <- reads s
        = [(Partial Nothing j, s)]
    readsPrec _ s
        | [(i,'/':'2':'^':s)] <- reads s
        , [(j,s)] <- reads s
        = [(Partial (Just i) j, s)]
    readsPrec _ _ = []

compute
  :: Bool -> Bool -> Maybe Method -> Bool -> Bool -> Bool -> Int -> Maybe FilePath -> Maybe FilePath -> Maybe Int -> Int -> Int -> Int
  -> Maybe FilePath -> Either FilePath [[String]] -> Maybe Int -> Partial -> IO ()
compute del transp met mult printmat silent guess levelin levelout level_ tra uroll ali out fname timeout (Partial tasks splitPower) = do
    gen <- newStdGen
    s <- either (fmap (filter (not . null) . map words . lines) . readFile) return fname

    levs <- case levelin of
        Nothing -> return []
        Just f -> map read . lines <$> readFile f

    let vs = map (map read) s :: [[Integer]]

        mat_ = (if transp then transpose' else id)
            . (if del then transpose . filter (any (/=0)) . transpose . filter (any (/=0)) else id)
            $ map fromIntegral <$> vs

        transpose' xs = if length xs <= length t then xs else t
          where
            t = transpose xs

        ns@(~[width]) = nub $ length <$> mat_
        output = maybe putStrLn writeFile out

        method = case met of
            Nothing -> id
            Just Mnosort -> id
            Just Masum  -> sortByASum
            Just Mb     -> sortByASum'
            Just (Mrandom mi) -> \xs -> shuffle' xs (length xs) $ maybe gen mkStdGen mi

        mat = (if mult then multguess else id)
            . method
            $ mat_

        nx = length mat - splitPower
        level = fromMaybe (min (nx - length levs - 1) $ round $ fromIntegral nx / 4) level_

    checkPred "matix row lengths differ" $ length ns == 1
    checkPred "number of rows should be less than 129" $ nx <= 128
    checkPred "alignment should be 1,2,4,8,16,..." $ ali `elem` map (2^) [0..10]
    checkPred "unroll should be greater than 1" $ uroll >= 1
--    checkPred ("unroll " ++ show uroll ++ " less or equal then the width " ++ show width $ uroll <= width
    checkPred "level should not be negative" $ level >= 0
    checkPred "level should be less than the number of rows" $ level < nx
    checkPred "trace level should not be negative" $ tra >= 0
    checkPred "trace level should be less or equal than level" $ tra <= level

    when (not silent) $ do
        when (asum (concat vs) >= 2^31) $ output "!!! warning: overflow may happen !!!"
        output $ show nx ++ " x " ++ show width
        output $ "levels: " ++ show level

    let tasks' = maybe [1..2^splitPower] pure tasks

    resChan <- newChan

    tids <- forM tasks' \i -> forkIO $ do

        let mat' = mkPartial (2^splitPower-i) splitPower mat

        when (not silent && printmat && i == head tasks') $ output $ showMat mat'

        (res, levs') <- umes_ guess (not silent) ali ali tra uroll level levs mat'

        case levelout of
            Just f | i == head tasks' -> writeFile f $ unlines $ map show levs'
            _ -> return ()

        writeChan resChan res

    case timeout of
        Nothing -> return ()
        Just t -> void $ forkIO $ do
            threadDelay $ 1000000 * t
            forM_ tids killThread

    ress <- forM tasks' \_ -> readChan resChan
    output $ show $ maximum ress

mkPartial i 0 mat = mat
mkPartial i j (x: y: mat) = mkPartial (i`div`2) (j-1) (zipWith (if odd i then (+) else (-)) x y: mat)

timeRandom :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
timeRandom r y y' sy x x' sx (fromIntegral -> z) (fromIntegral -> z') rep = forM_ (f $ zip [x, x+sx..x'] [y, y+sy..y']) $ \((i, j), z) -> do
    putStr $ show (i,j,z) ++ "  " ++ show (log $ fromIntegral i) ++ "    "
    mats <- replicateM rep $ replicateM i $ replicateM j $ fromIntegral <$> randomRIO (-r, r)
    t1 <- getCurrentTime
    let opt = umes 0 False 8 8 0 1 z [] i j
    forM_ mats $ \mat -> fst <$> opt mat
    t2 <- getCurrentTime
    putStrLn $ show $ log $ (realToFrac (diffUTCTime t2 t1) :: Double) / fromIntegral rep
  where
    f xs = zip xs $ map round [z, z + (z' - z)/(fromIntegral (length xs) - 1) ..]

printTestMat :: Maybe FilePath -> Int -> IO ()
printTestMat out n = maybe putStrLn writeFile out $ showMat $ testMat n

testTestMat :: Maybe Int -> IO ()
testTestMat n_ = do
    res <- fst <$> umes 0 False 8 8 0 1 (length m `div` 4) [] (length m) (length $ head m) m
    case () of
      _ | f n == res -> putStrLn "OK"
        | otherwise -> error "fatal error"
  where
    n = fromMaybe 6 n_
    m = testMat n
    f 2 = 4
    f 3 = 36
    f 4 = 120
    f 5 = 280
    f 6 = 540
    f 7 = 924

testMat n = [[(if i == j then 1 else 3) * inner i j | i<-fs] | j<-fs]
  where
    fs = [ins b   1  $ ins a 1 $ replicate (n-2) 0 | a <- [0..n-2], b <- [a+1..n-1]]
      ++ [ins b (-1) $ ins a 1 $ replicate (n-2) 0 | a <- [0..n-2], b <- [a+1..n-1]]

    ins i a (splitAt i -> (as, bs)) = as ++ a: bs

showMat = unlines . map unwords . transpose . map (pad . map (g . show)) . transpose
  where
    pad xs = map f xs
      where
        f = take n . (++ repeat ' ')
        n = maximum $ 0: map length xs
    g s@('-': _) = s
    g s = ' ': s

sortByASum = map snd . sortBy (flip compare `on` fst) . map (asum &&& id)

asum = sum . map abs

inner a b = sum $ zipWith (*) a b

sortByASum' [] = []
sortByASum' xs = reverse $ a: f a as
  where
    (a, as) = snd $ maximumBy (flip compare `on` fst) $ map (asum . fst &&& id) $ getOut xs

    f _ [] = []
    f x xs = a: f a as
      where
        (a, as) = snd $ maximumBy (flip compare `on` fst) $ map (abs . inner x . fst &&& id) $ getOut xs

    getOut :: [a] -> [(a, [a])]
    getOut xs = [(x, as ++ bs) | (as, x:bs) <- zip (inits xs) (tails xs)]

multguess (x: xs) = x: f x xs
  where
    f _ [] = []
    f v (x: xs)
        | asum a >= asum b = x: f a xs
        | otherwise = map negate x: f b xs
      where
        a = zipWith (+) v x
        b = zipWith (-) v x


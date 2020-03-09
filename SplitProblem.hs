import Control.Monad
import System.Environment

main = do
    (s: ss) <- getArgs
    let i = read s
    forM_ [1..2^i] $ \j -> do
        putStrLn $ unwords $ "binaryopt": ss ++ ["--partial", show j ++ "/2^" ++ show i]


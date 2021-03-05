module Main (main) where
import Lib

main :: IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
main = makeRequest 
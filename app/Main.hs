module Main (main) where
import Lib

import Data.Map

import Data.Aeson

main :: IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
main = do
    currencies <- middlewareRequest
    print $ encode $ fromList currencies  
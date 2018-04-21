module Nix.Fetch where

import Control.Monad.IO.Class
import Nix.Value
import Data.Text
import  System.Directory
import qualified Pipes
import qualified Pipes.HTTP as HTTP
import System.Random

import Nix.Exec

fetch :: Text -> Maybe (NThunk m) -> IO (NValue m)
fetch uri msha = (print "Hello") >> return undefined -- undefined
-- liftIO $ do
--   req <- parseUrl 
--   runEffect $

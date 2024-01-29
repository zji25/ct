module Main (main) where

import HW5.Action (runHIO, HiPermission(..))
import HW5.Parser (parse)
import HW5.Evaluator (eval)
import HW5.Pretty (prettyValue, prettyError)
import Data.Set (fromList)
import Text.Megaparsec (errorBundlePretty)
import Control.Monad.IO.Class (liftIO)
import Prettyprinter.Render.Terminal (putDoc)
import GHC.TopHandler (runMainIO)

main :: IO ()
main = runMainIO loop 
  where 
    loop = putStr "hi> " >> getLine >>= \s ->
      case s of
        ":q" -> return ()
        _ -> do 
          case parse s of 
            Left eb -> putStrLn $ errorBundlePretty eb
            Right expr -> do 
              result <- liftIO $ runHIO (eval expr) $ fromList [AllowRead, AllowWrite, AllowTime]
              putDoc $ case result of
                Left err -> prettyError err
                Right val -> prettyValue val
              putStrLn ""
          loop

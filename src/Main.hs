module Main
    ( main )
where

import Data.Char                ( isSpace )
import Control.Monad.IO.Class   ( liftIO )
import System.Console.Haskeline

import Parser      ( parse )
import Compiler    ( compile )
import Interpreter ( interpret )

loop :: InputT IO ()
loop = do
    minput <- getInputLine ">> "
    case minput of
        Just ":q"    -> pure ()
        Just ":quit" -> pure ()
        Just input | not (all isSpace input) ->
            case parse input of
                Left err   -> outputStrLn err >> loop
                Right expr -> do
                    (Just val) <- liftIO $ interpret $ compile expr
                    outputStrLn $ show val
                    loop
        _ -> loop

main :: IO ()
main = runInputT defaultSettings loop

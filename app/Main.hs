


import           Data.CoNLL
import           Protolude
import           System.Environment
import           System.Exit
import           System.IO           hiding(getContents) 
import           Text.Show.Pretty          (ppShow)

main :: IO ()
main = do args <- getArgs
          case args of
            ["-indented"] -> main_ (toSL.ppShow)
            []            -> main_ show
            _             -> exitWithProblem $ "Invalid arguments: " <> show args



main_ :: ([CorenlpTree Text] -> Text) -> IO ()
main_ printer = do input <- getContents -- read lazily
                   either (exitWithProblem . show) (putText . printer)
                        $ parseCorenlpTrees input


-- | Prints to stderr and exit with (-1) exit code
exitWithProblem :: Text -> IO ()
exitWithProblem err = do hPutStrLn stderr $ toSL err
                         exitFailure


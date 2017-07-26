

module Data.CoNLLSpec where


import           Data.CoNLL
import           Protolude
import           System.Directory
import           Test.Tasty.Hspec
import           Data.List         (isSuffixOf)
---------------------------------------------------------------------------



spec :: Spec
spec = describe "the conll parser"                $ do

          it "must parse every example file"      $ do
            gatherErrFromParsing "examples" `shouldReturn` []
          
          it "must produce the expected output"   $ do
            pendingWith "TODO"



gatherErrFromParsing :: FilePath -> IO [SyntaxErrorCoNLL]
gatherErrFromParsing path = catMaybes <$> (parseEveryFile =<< getEveryConllFile path)

getEveryConllFile :: FilePath -> IO [FilePath]
getEveryConllFile path = do xs <- getDirectoryContents path
                            return . fmap ((path<>"/")<>) $ filter (isSuffixOf ".conll") xs


parseEveryFile   :: [FilePath] -> IO [Maybe SyntaxErrorCoNLL]
parseEveryFile = mapM $ fmap (either Just (const Nothing) .parseCorenlpTrees) . readFile
-- fmap parseCorenlpTrees . readFile path


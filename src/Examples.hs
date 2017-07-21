
{-# LANGUAGE DeriveAnyClass #-}

module Examples where


import           Data.CoNLL
import           Data.ParsedSentence
import           Protolude
import           Text.Show.Pretty          (ppShow)
-- | This module is only to print the examples, so it can be manually check it worked as expected.





-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO: move to another file.

-- | One example file may contain 0 or more `ParsedSentence`s
getExample :: FilePath -> IO (Either SyntaxErrorCoNLL [CorenlpTree Text])
getExample path = parseCorenlpTrees <$> readFile path


executeExample :: FilePath -> IO ()
executeExample path =  either print (mapM_ $ putText . prettyPrint) =<< getExample path

executeExample_inputText :: IO () 
executeExample_inputText = executeExample "examples/inputs.txt.conll"


executeExample_judgementHtml :: IO () 
executeExample_judgementHtml = executeExample "examples/judgement.html.conll"


prettyPrint :: CorenlpTree Text -> Text
prettyPrint = toSL . ppShow




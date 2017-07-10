
module Data.CoNLL where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Protolude
import qualified Data.Text as T

{- Module to define how to parse corenlp conll output files. 
-}


data CorenlpCoNLL = CorenlpCoNLL
         { _outFileOrdIndex :: Int
         , _outFileToken    :: Text
         , _outFileLemma    :: Text
         , _outFilePOS      :: Text
         , _outFileNER      :: Text
         , _outFileHead     :: Int
         , _outFileDepRel   :: Text
         } deriving(Show,Read,Eq,Ord)



parseConllOutput :: Text -> Maybe [[CorenlpCoNLL]]
parseConllOutput fileContent = traverse formatGroup $ wordsBy (T.all isSpace) (T.lines fileContent)

formatGroup :: [Text] -> Maybe [CorenlpCoNLL]
formatGroup = traverse formatLine


formatLine :: Text -> Maybe CorenlpCoNLL
formatLine l = case T.splitOn "\t" l of
                [  outFileOrdIndex
                 , _outFileToken
                 , _outFileLemma
                 , _outFilePOS
                 , _outFileNER
                 , outFileHead
                 , _outFileDepRel
                 ]                  -> do _outFileOrdIndex <- readMaybe $ toSL outFileOrdIndex
                                          _outFileHead     <- readMaybe $ toSL outFileHead
                                          return CorenlpCoNLL{..}

                _                   -> do Nothing

-- TODO: use an actual TSV libraries so is resillent to corner cases (escapes problematic values).


module Data.CoNLL where

import           Data.Char
import           Data.Label
import           Data.List
import           Data.List.Split
import           Protolude
import qualified Data.Text as T

{- Module to define how to parse corenlp conll output files. 
-}


data CorenlpCoNLL pos rel = CorenlpCoNLL
         { _outFileOrdIndex :: Int
         , _outFileToken    :: Text
         , _outFileLemma    :: Text
         , _outFilePOS      :: pos
         , _outFileNER      :: Text
         , _outFileHead     :: Int
         , _outFileDepRel   :: rel
         } deriving(Show,Read,Eq,Ord)



parseConllOutput :: (TagLabel rel, TagLabel pos) => Text -> Maybe [[CorenlpCoNLL rel pos]]
parseConllOutput fileContent = traverse formatGroup $ wordsBy (T.all isSpace) (T.lines fileContent)

formatGroup :: (TagLabel rel, TagLabel pos) => [Text] -> Maybe [CorenlpCoNLL rel pos]
formatGroup = traverse formatLine


formatLine :: (TagLabel rel, TagLabel pos) => Text -> Maybe (CorenlpCoNLL rel pos)
formatLine l = case T.splitOn "\t" l of
                [  outFileOrdIndex
                 , _outFileToken
                 , _outFileLemma
                 , outFilePOS
                 , _outFileNER
                 , outFileHead
                 , outFileDepRel
                 ]                  -> do _outFileOrdIndex <- readMaybe $ toSL outFileOrdIndex
                                          _outFileHead     <- readMaybe $ toSL outFileHead
                                          _outFileDepRel   <- fromLabelText outFileDepRel 
                                          _outFilePOS      <- fromLabelText outFilePOS
                                          return CorenlpCoNLL{..}

                _                   -> do Nothing

-- TODO: use an actual TSV libraries so is resillent to corner cases (escapes problematic values).

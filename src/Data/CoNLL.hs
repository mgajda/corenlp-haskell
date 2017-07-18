
module Data.CoNLL ( CorenlpCoNLL(..)
                  , SyntaxErrorCoNLL(..)
                  , parseConllOutput
                  ) where

import           Data.Char
import           Data.Label
import           Data.List
import           Data.List.Split
import           Protolude
import qualified Data.Text as T

{- Module to define how to parse corenlp conll output files. 
-}

-- | Raw representation of a line on a `CoNLL` file.
data CorenlpCoNLL pos rel = CorenlpCoNLL
         { _outFileOrdIndex :: Int
         , _outFileToken    :: Text
         , _outFileLemma    :: Text
         , _outFilePOS      :: pos
         , _outFileNER      :: Text
         , _outFileHead     :: Int
         , _outFileDepRel   :: rel
         } deriving(Show,Read,Eq,Ord)

-- | SyntaxErrorCoNLL:  Reason                      LineNumber Culprit
--     |                 |                             |         |
--     v                 v                             v         v
data SyntaxErrorCoNLL = UnkonwnPosTag                 Int       Text 
                      | UnkwownRelTag                 Int       Text 
                      | CoulNotParseInteger           Int       Text 
                      | InvalidNumberOfElementsOnLine Int       Text 
                      | TheresNoRoot
                      deriving(Show,Read,Eq,Ord)



parseConllOutput :: (TagLabel rel, TagLabel pos) => Text -> Either SyntaxErrorCoNLL [[CorenlpCoNLL rel pos]]
parseConllOutput fileContent = traverse formatGroup $ wordsBy (T.all isSpace . snd) (zip [1..] $ T.lines fileContent)

formatGroup :: (TagLabel rel, TagLabel pos) => [(Int,Text)] -> Either SyntaxErrorCoNLL [CorenlpCoNLL rel pos]
formatGroup = traverse formatLine


formatLine :: (TagLabel rel, TagLabel pos) => (Int,Text) -> Either SyntaxErrorCoNLL (CorenlpCoNLL rel pos)
formatLine (n,l) = case T.splitOn "\t" l of
                [  outFileOrdIndex
                 , _outFileToken
                 , _outFileLemma
                 , outFilePOS
                 , _outFileNER
                 , outFileHead
                 , outFileDepRel
                 ]                  -> do _outFileOrdIndex <- parsingOn outFileOrdIndex (readMaybe.toSL)  CoulNotParseInteger 
                                          _outFileHead     <- parsingOn outFileHead     (readMaybe.toSL)  CoulNotParseInteger
                                          _outFileDepRel   <- parsingOn outFileDepRel   fromLabelText     UnkwownRelTag
                                          _outFilePOS      <- parsingOn outFilePOS      fromLabelText     UnkonwnPosTag
                                          return CorenlpCoNLL{..}

                _                   -> do Left $ InvalidNumberOfElementsOnLine n l
  where
    parsingOn :: Text -> (Text -> Maybe a) -> (Int -> Text -> SyntaxErrorCoNLL) -> Either SyntaxErrorCoNLL a
    parsingOn x parser errDesc  = note (errDesc n x) (parser x)
-- TODO: use an actual TSV libraries so is resillent to corner cases (escapes problematic values).


module Data.CoNLL ( CorenlpCoNLL
                  , CorenlpTree
                  , SyntaxErrorCoNLL(..)
                  , parseConllOutput
                  , parseCorenlpTrees
                  , module Data.ConllToken
                  , module Model.UniversalTreebank
                  , module Data.SyntaxTree
                  ) where

import           Data.Char
import           Data.ConllToken (ConllToken(..),SyntaxErrorCoNLL(..))
import           Data.List
import           Data.List.Split
import           Data.SyntaxTree (SyntaxtTree(..),createSyntaxTree)
import           Data.TagLabel
import           Model.UniversalTreebank
import           Protolude
import qualified Data.Text as T

{- Module to define how to parse corenlp conll output files. 
-}

-- | Raw representation of a line on a `CoNLL` file.

type CorenlpCoNLL a = ConllToken POS () REL () a
type CorenlpTree  a = SyntaxtTree POS () REL () a


parseCorenlpTrees :: Text -> Either SyntaxErrorCoNLL [CorenlpTree Text]
parseCorenlpTrees txt = traverse createSyntaxTree =<< parseConllOutput txt

parseConllOutput :: Text -> Either SyntaxErrorCoNLL [[CorenlpCoNLL Text]]
parseConllOutput fileContent = traverse formatGroup $ wordsBy (T.all isSpace . snd) (zip [1..] $ T.lines fileContent)

formatGroup :: [(Int,Text)] -> Either SyntaxErrorCoNLL [CorenlpCoNLL Text]
formatGroup = traverse formatLine


formatLine :: (Int,Text) -> Either SyntaxErrorCoNLL (CorenlpCoNLL Text)
formatLine (n,l) = case T.splitOn "\t" l of
                [  tnId
                 , _tnWord
                 , _tnLemma
                 , tnPosCG
                 , _ -- omitting NER TODO
                 , tnHead
                 , tnRel
                 ]                  -> do _tnId     <- parsingOn tnId    (readMaybe.toSL)  CoulNotParseInteger 
                                          _tnHead   <- parsingOn tnHead  (readMaybe.toSL)  CoulNotParseInteger
                                          _tnRel    <- parsingOn tnRel   fromLabelText     UnkwownRelTag
                                          _tnPosCG  <- parsingOn tnPosCG fromLabelText     UnkonwnPosTag
                                          
                                          return ConllToken{ _tnPosFG    = ()
                                                           , _tnFeats    = ()
                                                           , _tnHeadProj = ""
                                                           , _tnRelProj  = ""
                                                           , ..
                                                           }

                _                   -> do Left $ InvalidNumberOfElementsOnLine n l
  where
    parsingOn :: Text -> (Text -> Maybe a) -> (Int -> Text -> SyntaxErrorCoNLL) -> Either SyntaxErrorCoNLL a
    parsingOn x parser errDesc  = note (errDesc n x) (parser x)
-- TODO: use an actual TSV libraries so is resillent to corner cases (escapes problematic values).

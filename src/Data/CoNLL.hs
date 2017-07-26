{-# LANGUAGE DeriveAnyClass #-}
module Data.CoNLL ( CorenlpCoNLL
                  , CorenlpTree
                  , SyntaxErrorCoNLL(..)
                  , parseConllOutput
                  , parseCorenlpTrees
                  , module Data.ConllToken
                  , module Model.UniversalTreebank
                  , module Model.PennTreebank
                  , module Data.SyntaxTree
                  ) where

import           Data.Char
import           Data.ConllToken (ConllToken(..),SyntaxErrorCoNLL(..))
import           Data.List
import           Data.List.Split
import           Data.SyntaxTree (SyntaxtTree(..),createSyntaxTree)
import           Data.TagLabel
import           Model.UniversalTreebank
import           Model.PennTreebank
import           Protolude
import qualified Data.Text as T

{- Module to define how to parse corenlp conll output files. 
-}

-- | Raw representation of a line on a `CoNLL` file.

type CorenlpCoNLL a = ConllToken POS () REL NER a
type CorenlpTree  a = SyntaxtTree POS () REL NER a

-- | Named Entity Recognition
data NER = O
         | CARDINAL 
         | DATE
         | DURATION
         | FACILITY 
         | GPE
         | LOCATION 
         | MEASURE
         | MISC
         | MONEY
         | NUMBER
         | ORDINAL
         | ORGANIZATION 
         | PERCENT 
         | PERSON 
         | SET
         | TIME
         deriving(Show,Eq,Read,Ord,Generic,TagLabel)



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
                 , ner -- omitting NER TODO
                 , tnHead
                 , tnRel
                 ]                  -> do _tnId     <- parsingOn tnId    (readMaybe.toSL)  CoulNotParseInteger 
                                          _tnHead   <- parsingOn tnHead  (readMaybe.toSL)  CoulNotParseInteger
                                          _tnRel    <- parsingOn tnRel   fromLabelText     UnkwownRelTag
                                          _tnPosCG  <- parsingOn tnPosCG fromLabelText     UnkonwnPosTag
                                          _tnFeats  <- parsingOn ner     fromLabelText     UnkwownRelTag
                                          
                                          return ConllToken{ _tnPosFG    = ()
                                                           , _tnHeadProj = ""
                                                           , _tnRelProj  = ""
                                                           , ..
                                                           }

                _                   -> do Left $ InvalidNumberOfElementsOnLine n l
  where
    parsingOn :: Text -> (Text -> Maybe a) -> (Int -> Text -> SyntaxErrorCoNLL) -> Either SyntaxErrorCoNLL a
    parsingOn x parser errDesc  = note (errDesc n x) (parser x)
-- TODO: use an actual TSV libraries so is resillent to corner cases (escapes problematic values).











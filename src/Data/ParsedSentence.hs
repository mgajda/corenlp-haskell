

module Data.ParsedSentence where


import           Protolude
import           Data.CoNLL



data ParsedSentence model = ParsedSentence
        { _leftRelations   :: GrammarArc  model
        , _token           :: ParsedToken model
        , _rightRelations  :: GrammarArc  model
        } deriving(Generic)

data ParsedToken model = ParsedToken
        { _tokFinePOS      :: FinePOS   model -- ^ For example `DT`
        , _tokCoarsePOS    :: CoarsePOS model -- ^ For example `DET`
        , _tokToken        :: Token     model -- ^ The token after normalization, for example `"which"`
        , _tokSentencePart :: Sentence  model -- ^ The text segment from where it got extracted, for example `Which, `
        } deriving(Generic)


type GrammarArc model = [( GrammRel model, ParsedSentence model)]

type CommonConst a = (Eq a,Read a,Show a)

class NlpModel model where

  type FinePOS   model :: *
  type CoarsePOS model :: *
  type Token     model :: *
  type Sentence  model :: *
  type GrammRel  model :: *
  

{-# LANGUAGE DeriveAnyClass #-}

module Examples where


import           Data.ParsedSentence
import           Protolude
import           Text.Show.Pretty          (ppShow)

-- | This module is only to print the examples, so it can be manually check it worked as expected.





-- These are just to parse an example. (I've changed a couple of them, like the one for question mark
-- to simplify it.
data POS = CC
         | CD
         | Colon       (SpelledAs ":"    )
         | DT
         | EX
         | IN
         | JJ
         | JJR
         | LRB_        (SpelledAs "-LRB-")
         | LS
         | MD
         | NN
         | NNP
         | NNPS
         | NNS
         | POS
         | PRP
         | PRP_Dollar  (SpelledAs "PRP$")
         | Punctuation (SpelledAs ".") -- ^ Not just dots, at least it also includes question marks
         | Quotes      (SpelledAs "''")
         | Quotes2     (SpelledAs "``")
         | RB
         | RBR
         | RP
         | RRB_        (SpelledAs "-RRB-")
         | Semicolon   (SpelledAs ",")
         | TO
         | VB
         | VBD
         | VBG
         | VBN
         | VBP
         | VBZ
         | WDT
         | WP_Dollar     (SpelledAs "WP$")
         | WRB
         deriving(Show,Read,Eq,Ord,Generic,TagLabel)

data REL = Acl
         | ACL_RELCL     (SpelledAs "acl:relcl"   )
         | Advcl
         | Advmod
         | Amod
         | Appos
         | Aux
         | Auxpass
         | Case
         | Cc
         | Cc_preconj    (SpelledAs "cc:preconj"  )
         | Ccomp
         | Compound
         | Compound_PRT  (SpelledAs "compound:prt")
         | Conj
         | Cop
         | Csubj
         | Dep
         | Det
         | Dobj
         | Expl
         | Iobj
         | Mark
         | Mwe
         | Neg
         | Nmod
         | Nmod_npmod    (SpelledAs "nmod:npmod"  )
         | NMOD_POS      (SpelledAs "nmod:poss"   )
         | NMOD_TMOD     (SpelledAs "nmod:tmod"   )
         | Nsubj
         | Nsubjpass
         | Nummod
         | Parataxis
         | Punct
         | ROOT
         | Xcomp
         deriving(Show,Read,Eq,Ord,Generic,TagLabel)



-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO: move to another file.

-- | One example file may contain 0 or more `ParsedSentence`s
getExample :: FilePath -> IO (Either SyntaxErrorCoNLL [ParsedSentence POS REL])
getExample path = parseConNll <$> readFile path


executeExample :: FilePath -> IO ()
executeExample path =  either print (mapM_ $ putText . prettyPrint) =<< getExample path

executeExample_inputText :: IO () 
executeExample_inputText = executeExample "examples/inputs.txt.conll"


executeExample_judgementHtml :: IO () 
executeExample_judgementHtml = executeExample "examples/judgement.html.conll"

-- | We do not print the whole parsed sentece as it would be way too redundant (as it shows each node twice, and for each time, all 
--   its children). Instead we show the syntax-tree starting from the root-node.
prettyPrint :: (Show a,Show b) => ParsedSentence a b -> Text
prettyPrint = toSL . ppShow . _rootNode







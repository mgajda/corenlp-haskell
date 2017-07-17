
{-# LANGUAGE DeriveAnyClass #-}

module Data.ParsedSentence ( ParsedSentence(..)
                           , ParsedToken(..)
                           , SyntaxNode(..)
                           , parseConNll
                           , example
                           ) where


import           Data.CoNLL
import           Data.CoNLL
import           Data.Label
import           Data.Map
import           Protolude
import           Text.Show.Pretty          (ppShow)

data SyntaxNode pos rel = SyntaxNode
       { _token         :: ParsedToken pos 
       , _sentenceIndex :: Int
       , _relations     :: [(rel,SyntaxNode pos rel)] 
       } deriving(Show,Read,Eq,Generic,Ord)

data ParsedSentence pos rel = ParsedSentence
       { _rootNode    :: SyntaxNode pos rel
       , _indexToNode :: Map Int (SyntaxNode pos rel)
       , _headToNode  :: Map Int [(rel, SyntaxNode pos rel)] -- ^ All relations having as "head" the given index  
       } deriving(Show,Read,Eq,Generic,Ord)

data ParsedToken pos = ParsedToken
       { _tokenPOS      :: pos
       , _tokenText     :: Text
       , _tokenLemma    :: Text
       } deriving(Show,Read,Eq,Generic,Ord)



parseConNll :: (Label pos, Label rel) => Text -> Maybe [ParsedSentence pos rel]
parseConNll file = undefined --traverse parsedConNllSentence =<< parseConllOutput file

parsedConNllSentence :: [CorenlpCoNLL pos rel] -> Maybe (ParsedSentence pos rel)
parsedConNllSentence conllLines = do rootNode <- snd <$> listToMaybe (parsedSentenceFrom 0)
                                     return ParsedSentence
                                            { _rootNode    = rootNode
                                            , _indexToNode =      fst <$> indexToNode
                                            , _headToNode  = fmap fst <$> headToNode 
                                            }
  where


    indexToNode = fromList $ partial <$> conllLines


    headToNode  = fromListWith (++) [ (head_,[((rel,node),i)]) 
                                    | (i,(node,(head_,rel))) <- reverse $ assocs indexToNode
                                    ] -- ^ we do not need to "reverse", but that would make
                                      --   the result order closer to the original

    

    parsedSentenceFrom n = let relations = fromMaybe [] $ lookup n headToNode
                            
                            in [ ( rel
                                 , sentenceNode{ _relations = parsedSentenceFrom i
                                               } 
                                 )

                               | ((rel,sentenceNode),i) <- relations
                               ]



    partial CorenlpCoNLL{..} =  ( _outFileOrdIndex
                                , ( SyntaxNode
                                     { _token = ParsedToken
                                          { _tokenPOS    = _outFilePOS
                                          , _tokenText   = _outFileToken
                                          , _tokenLemma  = _outFileLemma
                                          }
                                     , _sentenceIndex = _outFileOrdIndex
                                     , _relations     = [] 
                                     }
                                  , ( _outFileHead, _outFileDepRel
                                    )
                                  )
                                )

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO: move to another file.

example :: FilePath -> IO ()
example path = do rawText <- readFile path 
                  let result :: Maybe [ParsedSentence POS REL]
                      result = traverse parsedConNllSentence =<< parseConllOutput rawText
                   
                  putStrLn $ ppShow result 


-- These are just to parse an example. (I've changed a couple of them, like the one for question mark
-- to simplify it.
data POS = CC
         | DT
         | IN
         | JJ
         | MD
         | NN
         | PRP
         | Question
         | Semicolon
         | TO
         | VB
         deriving(Show,Read,Eq,Ord,Generic,Label)

data REL = Acl
         | Amod
         | Aux
         | Case
         | Cc
         | Ccomp
         | Compound
         | Conj
         | Det
         | Dobj
         | Mark
         | Nmod
         | Nsubj
         | Punct
         | ROOT
         deriving(Show,Read,Eq,Ord,Generic,Label)


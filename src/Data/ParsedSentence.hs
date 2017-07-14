

module Data.ParsedSentence ( ParsedSentence(..)
                           , ParsedToken(..)
                           , parseConNll
                           ) where


import           Data.CoNLL
import           Data.CoNLL
import           Data.Label
import           Data.Map
import           Protolude


data ParsedSentence pos rel = ParsedSentence
       { _token         :: ParsedToken pos 
       , _sentenceIndex :: Int
       , _relations     :: [(rel,ParsedSentence pos rel)] 
       } deriving(Show,Read,Eq,Generic,Ord)

data ParsedToken pos = ParsedToken
       { _tokenPOS      :: pos
       , _tokenText     :: Text
       , _tokenLemma    :: Text
       } deriving(Show,Read,Eq,Generic,Ord)



parseConNll :: (Label pos, Label rel) => Text -> Maybe [ParsedSentence pos rel]
parseConNll file = traverse parsedConNllSentence =<< parseConllOutput file


parsedConNllSentence :: [CorenlpCoNLL pos rel] -> Maybe (ParsedSentence pos rel)
parsedConNllSentence conllLines = do (root,_,_) <- lookup 0 partialParsed
                                     return root{ _relations = parsedSentenceFrom 0
                                                }
  where


    partialParsed  = fromList $ partial <$> conllLines


    partialParsed' = fromListWith (++) [ (head_,[(x,i,rel)]) 
                                       | (i,(x,head_,rel)) <- assocs partialParsed
                                       ]


    

    parsedSentenceFrom n = let relations = fromMaybe [] $ lookup n partialParsed'
                            
                            in [ ( rel
                                 , sentenceNode{ _relations = parsedSentenceFrom i
                                               } 
                                 )

                               | (sentenceNode,i,rel) <- relations
                               ]



    partial CorenlpCoNLL{..} =  ( _outFileOrdIndex
                                , ( ParsedSentence
                                     { _token = ParsedToken
                                          { _tokenPOS    = _outFilePOS
                                          , _tokenText   = _outFileToken
                                          , _tokenLemma  = _outFileLemma
                                          }
                                     , _sentenceIndex = _outFileOrdIndex
                                     , _relations     = [] 
                                     }
                                  , _outFileHead
                                  , _outFileDepRel
                                  )
                                )


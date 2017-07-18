
module Data.ParsedSentence ( ParsedSentence(..)
                           , ParsedToken(..)
                           , SyntaxNode(..)
                           , parseConNll
                           , module Data.CoNLL
                           , module Data.Label
                           ) where


import           Data.CoNLL
import           Data.Label
import           Data.Map
import           GHC.TypeLits
import           Protolude

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



parseConNll :: (TagLabel pos, TagLabel rel) => Text -> Either SyntaxErrorCoNLL [ParsedSentence pos rel]
parseConNll file = traverse parsedConNllSentence =<< parseConllOutput file

parsedConNllSentence :: [CorenlpCoNLL pos rel] -> Either SyntaxErrorCoNLL (ParsedSentence pos rel)
parsedConNllSentence conllLines = do rootNode <- note TheresNoRoot 
                                               $ snd <$> listToMaybe (parsedSentenceFrom 0)
                                     
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


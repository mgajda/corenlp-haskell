

module Data.Label 
     ( Label(..)
     , fromLabel
     , toLabel
     ) 
 where




import           Data.Map
import           GHC.Generics
import           GHC.TypeLits
import           Protolude       hiding(fromLabel)
import qualified GHC.Generics    as G

class (Ord label) => Label label where
   labelMap ::  Map Text label

   default labelMap :: (Generic label, GLabel (Rep label) ) => Map Text label
   labelMap = fmap G.to . fromList $  gLabelMap (Proxy :: Proxy (Rep label))

   reverseLabelMap :: Map label Text
   reverseLabelMap = fromList [ (b,a) | (a,b) <- assocs labelMap]


fromLabel :: (Label label) => Text -> Maybe label
fromLabel t = lookup t labelMap

toLabel   :: (Label label) => label -> Text
toLabel l =  fromMaybe (error "imposible branch on Data.Label") 
          $  lookup l reverseLabelMap

{- | The class `Label` represents types to be used as labels for CoreNLP, such POS sets or gramatical relations.

     The class can be manually derived for special cases, but it is expected to be auto derived in most cases.

     First to enable autoderive, set:
     
     ```
     {-# LANGUAGE DeriveAnyClass #- }
     ```

     now you can use it on any enumeration already deriving `Ord` and `Generic`, for example defining:
     ```
     data Foo = Foo1
              | Foo2
              | Foo3
              deriving(Ord,Eq,Generic,Label)
     ```
     would make the type `Foo` to hold the labels `"Foo1"`, `"Foo2"`, `"Foo3"`.

     It is also posible to derive `Label` on _tree like_ types, for example defining:
     ```
     data Xoo = Xoo1
              | Xoo2
              | Xoo3
              deriving(Ord,Eq,Generic,Label)

     data Moo = Moo1
              | Moo2
              | Moo3
              deriving(Ord,Eq,Generic,Label)
     
     data Complex = Comp1
                  | Comp2
                  | AnyFoo Foo
                  | AnyXooOrMoo (Either Xoo Moo)
                  | Comp3
                  deriving(Ord,Eq,Generic,Label)
     ```
     would make the type `Complex` to hold the labels:
     . `"Comp1"`
     . `"Comp2"`
     - `"Foo1"`
     - `"Foo2"`
     - `"Foo3"`
     - `"Xoo1"`
     - `"Xoo2"`
     - `"Xoo3"`
     - `"Moo1"`
     - `"Moo2"`
     - `"Moo3"`
     - `"Comp3"`

     Notice all the _tree fingers_ appear as labels, but constructors `AnyFoo` or `AnyXooOrMoo` __do not__ .

     It is specially handy use `Either` as a sum of labels, forexample we can define
     ```
     type Adjective = Either AdjComparative AdjSuperlative
     ```

     If one try to define `Label` over a type where it makes no sense, the compiler will complain and __will not allow__ it.
     Some example of types that can not derive `Label`:

     ```
     data Foo = Foo1
              | FooN Int    -- `Int` can not be treat as a set of labels, hence `Foo` can't either

     data Zoo = Zoo Xoo Moo -- Only singletons or sum types can derive `Label`, here `Zoo` is a product
                            -- type

     data EmptyData         -- Empty types (Types without constructors, like `Void`) can not be derived
                            -- as `Labels`.

     data Woo = Woo Woo     -- WARNING: recursive and mutually recursive types __are able__ to derive `Label`,
                            -- but they __will fail__ at run time if treated as such.
     ```


-}

--------------------------------------------------------------------
--------------------------------------------------------------------
-- Private "default class" machinery:


class GLabel f where
    gLabelMap :: Proxy f -> [(Text, f a)]

-- | Definitions: `D1` (instance base on to type definition metadata...we can ignore it and focus on `f`)
instance (GLabel f) => GLabel (D1 meta f) where
    gLabelMap _ = second M1 <$> gLabelMap (Proxy :: Proxy f)


-- | The labels of sum types are the labels of its parts 
instance (GLabel f, GLabel g) => GLabel ( f :+: g) where
  gLabelMap _ =  ( second L1  <$> gLabelMap (Proxy :: Proxy f))
              ++ ( second R1  <$> gLabelMap (Proxy :: Proxy g))


-- | The label of a constructor without arguments is the constructor's name itself
instance (KnownSymbol constructor) => GLabel (C1 ('MetaCons constructor a b) U1) where
  gLabelMap _ = [( toSL $ symbolVal (Proxy :: Proxy constructor) , M1 U1)] 


instance (Label labelType) => GLabel (C1 meta1 (S1 meta2 (K1 meta3 labelType))) where
  gLabelMap _ = assocs $ (M1 . M1 . K1)  <$> labelMap


---------------------------------------------------------------------------------------
-- Standard labels:

instance (Label l1, Label l2)  => Label (Either l1 l2)  





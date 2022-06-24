module Data.Immutaly.Internal.Util.Composite
  ( Composite,
    item,
    fromList,
    toList,
  ) where

data Composite a = Item a | Comp [Composite a] deriving (Show, Eq)

instance Semigroup (Composite a) where
  Item a <> Comp cs = Comp $ Item a : cs
  c1 <> c2          = Comp [c1, c2]

instance Monoid (Composite a) where
  mempty = Comp []

instance Foldable Composite where
  foldr _ b (Comp []) = b
  foldr f b (Item a)  = f a b
  foldr f b (Comp cs) = foldr (flip $ foldr f) b cs

item :: a -> Composite a
item = Item

fromList :: [a] -> Composite a
fromList = Comp . fmap item

toList :: Composite a -> [a]
toList = foldr (:) []

module AOC.HuetTree where

import Data.Sequence (Seq ((:<|), (:|>)), (><))
import Data.Sequence qualified as Seq

-- An n-ary tree with elements of type `a`
data Tree a = Node a (Seq (Tree a))
  deriving (Eq, Show)

-- The Huet zipper for this tree
data HuetCtx a
  = Top
  | Hole {upCtx :: HuetCtx a, up :: a, lft :: Seq (Tree a), rght :: Seq (Tree a)}
  deriving (Eq, Show)

-- A tree with a position attached
data HuetTree a = HuetTree {posValue :: Tree a, pos :: HuetCtx a}
  deriving (Eq, Show)

-- Helpers
mkRoot :: a -> Seq (Tree a) -> HuetTree a
mkRoot v descendants = HuetTree (Node v descendants) Top

mkParent :: forall a. a -> HuetCtx a -> Seq (Tree a) -> Tree a -> Seq (Tree a) -> HuetTree a
mkParent v ctx lefts middle rights = HuetTree (Node v descendants) ctx
  where
    descendants :: Seq (Tree a)
    descendants = (lefts :|> middle) >< rights

mkChild :: Tree a -> HuetCtx a -> a -> Seq (Tree a) -> Seq (Tree a) -> HuetTree a
mkChild v upCtx u l r = HuetTree v $ Hole upCtx u l r

-- TREE BUILDING
singletonTree :: forall a. a -> HuetTree a
singletonTree a = mkRoot a Seq.Empty

--TODO
--insertAtEnd :: forall a . HuetTree a -> HuetTree a
--insertAtEnd t1 t2 =

-- TRAVERSALS --
leftUnsafe :: HuetTree a -> HuetTree a
leftUnsafe (HuetTree _ Top) = error "Can't go left at root of the tree"
leftUnsafe (HuetTree hereV (Hole upCtx upV (fartherLeftVs :|> immLeftV) rightVs)) =
  mkChild immLeftV upCtx upV fartherLeftVs (hereV :<| rightVs)
leftUnsafe (HuetTree _ (Hole _ _ Seq.Empty _)) =
  error "Can't go left, this is the leftmost child"

rightUnsafe :: HuetTree a -> HuetTree a
rightUnsafe (HuetTree _ Top) = error "Can't go right at root of the tree"
rightUnsafe (HuetTree hereT (Hole upCtx upV leftTs (immRightT :<| fartherRightTs))) =
  mkChild immRightT upCtx upV (leftTs :|> hereT) fartherRightTs
rightUnsafe (HuetTree _ (Hole _ _ _ Seq.Empty)) =
  error "Can't go right, this is the rightmost child"

upUnsafe :: forall a. HuetTree a -> HuetTree a
upUnsafe (HuetTree _ Top) = error "Can't go up at root of the tree"
upUnsafe (HuetTree hereT (Hole upCtx upV leftTs rightTs)) =
  mkParent upV upCtx leftTs hereT rightTs

downLeftUnsafe :: HuetTree a -> HuetTree a
downLeftUnsafe (HuetTree (Node _ Seq.Empty) _) = error "Can't go down-left at fork with no descendants"
downLeftUnsafe (HuetTree (Node v (leftChild :<| otherChildren)) ctx) =
  mkChild leftChild ctx v Seq.Empty otherChildren

downRightUnsafe :: HuetTree a -> HuetTree a
downRightUnsafe (HuetTree (Node _ Seq.Empty) _) = error "Can't go down-right at fork with no descendants"
downRightUnsafe (HuetTree (Node v (otherChildren :|> rightChild)) ctx) =
  mkChild rightChild ctx v otherChildren Seq.Empty

--ex :: HuetTree Int
--ex = mkChild (L $ Leaf 1) Top (Fork 0 Seq.Empty) (Seq.empty) (L (Leaf 2) :<| Seq.empty)

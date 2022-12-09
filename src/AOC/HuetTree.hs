module AOC.HuetTree
  ( HuetTree,
    Tree (..),
    getValue,
    getTree,
    subtrees,
    singleton,
    insertFirst,
    insertLast,
    goToRoot,
    goToChildUnsafe,
    goLeft,
    goLeftUnsafe,
    goRight,
    goRightUnsafe,
    goUp,
    goUpUnsafe,
    goDownLeft,
    goDownLeftUnsafe,
    goDownRight,
    goDownRightUnsafe,
  )
where

import Data.Sequence (Seq ((:<|), (:|>)), (><))
import Data.Sequence qualified as Seq

-- An n-ary tree with elements of type `a` in leaves and forks
data Tree a = Node {value :: a, children :: Seq (Tree a)}
  deriving (Eq, Show, Functor, Foldable)

-- The Huet zipper for this tree
data HuetCtx a
  = Top
  | Hole {upHole :: HuetCtx a, up :: a, lft :: Seq (Tree a), rght :: Seq (Tree a)}
  deriving (Eq, Show, Functor, Foldable)

-- A tree with a position attached
data HuetTree a = HuetTree {posValue :: Tree a, pos :: HuetCtx a}
  deriving (Eq, Show, Functor, Foldable)

-- Helpers
mkRoot :: a -> Seq (Tree a) -> HuetTree a
mkRoot v descendants = HuetTree (Node v descendants) Top

mkParent :: forall a. a -> HuetCtx a -> Seq (Tree a) -> Tree a -> Seq (Tree a) -> HuetTree a
mkParent v hole lefts middle rights = HuetTree (Node v descendants) hole
  where
    descendants :: Seq (Tree a)
    descendants = (lefts :|> middle) >< rights

mkChild :: Tree a -> HuetCtx a -> a -> Seq (Tree a) -> Seq (Tree a) -> HuetTree a
mkChild v upHole u l r = HuetTree v $ Hole upHole u l r

-- DECONSTRUCT
getTree :: forall a. HuetTree a -> Tree a
getTree (HuetTree t _) = t

getValue :: forall a. HuetTree a -> a
getValue (HuetTree (Node v _) _) = v

subtrees' :: forall a. Tree a -> Seq (Tree a)
subtrees' t@(Node _ children) = foldMap subtrees' children :|> t

subtrees :: forall a. HuetTree a -> Seq (Tree a)
subtrees = subtrees' . getTree . goToRoot

-- TREE BUILDING
singleton :: forall a. a -> HuetTree a
singleton a = mkRoot a Seq.Empty

insertLast :: forall a. HuetTree a -> a -> HuetTree a
insertLast (HuetTree (Node b children) pos) a =
  HuetTree (Node b $ children :|> Node a Seq.Empty) pos

insertFirst :: forall a. HuetTree a -> a -> HuetTree a
insertFirst (HuetTree (Node b children) pos) a =
  HuetTree (Node b $ Node a Seq.Empty :<| children) pos

-- TODO: Operations for combining trees

-- TRAVERSALS --
goToRoot :: forall a. HuetTree a -> HuetTree a
goToRoot t = maybe t goToRoot $ goUp t

goToChildUnsafe :: forall a. Eq a => a -> HuetTree a -> HuetTree a
goToChildUnsafe v t = go $ goDownLeftUnsafe t
  where
    go :: HuetTree a -> HuetTree a
    go child
      | getValue child == v = child
      | otherwise = go $ goRightUnsafe child

goLeftUnsafe :: HuetTree a -> HuetTree a
goLeftUnsafe (HuetTree _ Top) = error "Can't go left at root of the tree"
goLeftUnsafe (HuetTree hereV (Hole upHole upV (fartherLeftVs :|> immLeftV) rightVs)) =
  mkChild immLeftV upHole upV fartherLeftVs (hereV :<| rightVs)
goLeftUnsafe (HuetTree _ (Hole _ _ Seq.Empty _)) =
  error "Can't go left, this is the leftmost child"

goLeft :: HuetTree a -> Maybe (HuetTree a)
goLeft (HuetTree _ Top) = Nothing
goLeft (HuetTree hereV (Hole upHole upV (fartherLeftVs :|> immLeftV) rightVs)) =
  Just $ mkChild immLeftV upHole upV fartherLeftVs (hereV :<| rightVs)
goLeft (HuetTree _ (Hole _ _ Seq.Empty _)) = Nothing

goRightUnsafe :: HuetTree a -> HuetTree a
goRightUnsafe (HuetTree _ Top) = error "Can't go right at root of the tree"
goRightUnsafe (HuetTree hereT (Hole upHole upV leftTs (immRightT :<| fartherRightTs))) =
  mkChild immRightT upHole upV (leftTs :|> hereT) fartherRightTs
goRightUnsafe (HuetTree _ (Hole _ _ _ Seq.Empty)) =
  error "Can't go right, this is the rightmost child"

goRight :: HuetTree a -> Maybe (HuetTree a)
goRight (HuetTree _ Top) = Nothing
goRight (HuetTree hereT (Hole upHole upV leftTs (immRightT :<| fartherRightTs))) =
  Just $ mkChild immRightT upHole upV (leftTs :|> hereT) fartherRightTs
goRight (HuetTree _ (Hole _ _ _ Seq.Empty)) = Nothing

goUpUnsafe :: forall a. HuetTree a -> HuetTree a
goUpUnsafe (HuetTree _ Top) = error "Can't go up at root of the tree"
goUpUnsafe (HuetTree hereT (Hole upHole upV leftTs rightTs)) =
  mkParent upV upHole leftTs hereT rightTs

goUp :: forall a. HuetTree a -> Maybe (HuetTree a)
goUp (HuetTree _ Top) = Nothing
goUp (HuetTree hereT (Hole upHole upV leftTs rightTs)) =
  Just $ mkParent upV upHole leftTs hereT rightTs

goDownLeftUnsafe :: HuetTree a -> HuetTree a
goDownLeftUnsafe (HuetTree (Node _ Seq.Empty) _) = error "Can't go down-left at fork with no descendants"
goDownLeftUnsafe (HuetTree (Node v (leftChild :<| otherChildren)) hole) =
  mkChild leftChild hole v Seq.Empty otherChildren

goDownLeft :: HuetTree a -> Maybe (HuetTree a)
goDownLeft (HuetTree (Node _ Seq.Empty) _) = Nothing
goDownLeft (HuetTree (Node v (leftChild :<| otherChildren)) hole) =
  Just $ mkChild leftChild hole v Seq.Empty otherChildren

goDownRightUnsafe :: HuetTree a -> HuetTree a
goDownRightUnsafe (HuetTree (Node _ Seq.Empty) _) = error "Can't go down-right at fork with no descendants"
goDownRightUnsafe (HuetTree (Node v (otherChildren :|> rightChild)) hole) =
  mkChild rightChild hole v otherChildren Seq.Empty

goDownRight :: HuetTree a -> Maybe (HuetTree a)
goDownRight (HuetTree (Node _ Seq.Empty) _) = Nothing
goDownRight (HuetTree (Node v (otherChildren :|> rightChild)) hole) =
  Just $ mkChild rightChild hole v otherChildren Seq.Empty

-- example tree
--ex :: HuetTree Integer
--ex = insertLast (insertLast (singleton 0) 1) 2

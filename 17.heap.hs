-- The Haskell standard library doesn't have a heap. A heap is a tree like data
-- structure where each root is guaranteed to be <= its children (The heap
-- property). The trick with performant heaps is to try and get them to be
-- balanced (i.e. get their children to be around the same height), and various
-- heap implementations do it in various ways.
--
-- Note that the standard library's Map is balanced and already provides O (log
-- n) minView and maxView, which might already be all you need.
--
-- Here we demonstrate an simple unbalanced ("skewed") that still has O (log n)
-- amortized bounds. This brilliant data structure is by the venerable Tarjan
-- himself. Here I use the implementation described in MonadReader 16
-- (http://themonadreader.files.wordpress.com/2010/05/issue16.pdf).
--
-- The idea is - 1. Union the heap with the larger root to the right child of
-- the other heap, and then 2. Swap the children of the resultant heap.
-- Magically, but provably, this gives us the amortized O (log n).

data Heap a = Empty | Heap a (Heap a) (Heap a)

union :: Ord a => Heap a -> Heap a -> Heap a
union h Empty = h
union Empty h = h
union hl@(Heap l ll lr) hr@(Heap r _ _)
  | l <= r = Heap l (union lr hr) ll
  | otherwise = union hr hl

extractMin :: Ord a => Heap a -> Maybe (a, Heap a)
extractMin Empty = Nothing
extractMin (Heap x l r) = Just (x, union l r)

singleton :: a -> Heap a
singleton x = Heap x Empty Empty

insert :: Ord a => a -> Heap a -> Heap a
insert x h = singleton x `union` h

-- Test by creating a heap, and popping them to see that they're in order.
main :: IO ()
main = (print . toList . fromList) testList
  where
    testList = [4, 8, 3, 1, 9, 0, 5, 6, 2, 7]
    fromList = foldr insert Empty
    toList h = case extractMin h of
      Nothing -> []
      Just (x, h') -> x : toList h'

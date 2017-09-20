module MeanDiff.Tree

type Tree<'a, 'b> =
  | LeafNode of 'a
  | InternalNode of 'b * Tree<'a, 'b> * Tree<'a, 'b>

val treeFoldPostorder :
  ('a -> 'c) -> ('c -> 'c -> 'b -> 'c) -> Tree<'a, 'b> -> 'c

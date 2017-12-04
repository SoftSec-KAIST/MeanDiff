module MeanDiff.Tree

type Tree<'a, 'b> =
  | LeafNode of 'a
  | InternalNode of 'b * Tree<'a, 'b> * Tree<'a, 'b>

let rec treeFoldPostorder foldFuncLeaf foldFuncInternal = function
  | LeafNode (leafData) ->
      foldFuncLeaf leafData
  | InternalNode (internalData, leftTree, rightTree) ->
      let leftAcc =
        leftTree |> treeFoldPostorder foldFuncLeaf foldFuncInternal
      let rightAcc =
        rightTree |> treeFoldPostorder foldFuncLeaf foldFuncInternal
      foldFuncInternal leftAcc rightAcc internalData

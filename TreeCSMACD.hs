module TreeCSMACD where

import Interface( MsgResult(..) )

data BTree a = Nil
             | Node a (BTree a) (BTree a)
             deriving (Eq, Show)

type TreeData = (Int, MsgResult)
type Tree' = BTree TreeData

data Tree = EmptyTree
          | EmptyLeaf Int
          | SuccessLeaf Int
          | ConflictNode Int Tree Tree
          | Unknown Int
          deriving (Eq, Show)

buildTree :: MsgResult -> Tree -> Tree
buildTree Success EmptyTree = EmptyTree
buildTree Empty EmptyTree = EmptyTree
buildTree Conflict EmptyTree = ConflictNode 0 (Unknown 1) (Unknown 2)

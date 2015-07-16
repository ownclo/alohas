module TreeCSMACD
    ( Tree
    , buildTreeM
    , isResolved
    ) where

import Control.Applicative( (<$>) )
import Control.Monad( foldM )

import Data.Maybe( isNothing )

import Interface( MsgResult(..) )

data Tree = ELeaf
          | SLeaf
          | CNode Tree Tree
          | Undef
          deriving (Eq, Show)

buildTree :: Tree -> MsgResult -> Maybe Tree
buildTree ELeaf _ = Nothing
buildTree SLeaf _ = Nothing
buildTree Undef r = Just $ newNode r
buildTree (CNode a b) r  =
    case buildTree a r of
      Just a' -> Just $ CNode a' b
      Nothing -> CNode a <$> buildTree b r

buildTreeM :: Maybe Tree -> MsgResult -> Maybe Tree
buildTreeM Nothing Conflict = Just newCNode
buildTreeM Nothing _        = Nothing
buildTreeM (Just t) res     = buildTree t res

treeFromResults :: [MsgResult] -> Maybe Tree
treeFromResults [] = Nothing
treeFromResults (r:rs) = do
    t <- buildTreeM Nothing r
    foldM buildTree t rs

newNode :: MsgResult -> Tree
newNode Success  = SLeaf
newNode Empty    = ELeaf
newNode Conflict = newCNode

newCNode :: Tree
newCNode = CNode Undef Undef

-- A conflict is resolved if there is no more
-- UNDEF leaves. It can be proven by induction
-- on a structure of a tree that
-- buildTree t * == Nothing <=> there is no more UNDEFS
-- => tree is resolved
isResolved :: Maybe Tree -> Bool
isResolved Nothing = True
isResolved (Just t) = isNothing $ buildTree t Success

-- testing
c, s, e :: MsgResult
c = Conflict
s = Success
e = Empty

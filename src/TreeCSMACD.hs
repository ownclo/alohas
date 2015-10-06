module TreeCSMACD
    ( Tree
    , Label
    , initLabel
    , initTree
    , canTransmit
    , decideTransmit
    , updateTree
    , isResolved
    -- testing
    ,treeFromResults
    ) where

import Control.Applicative
import Control.Monad( guard )

import Data.Maybe( isNothing )

import Interface( MsgResult(..) )

initTree :: Maybe Tree
initTree = Nothing

initLabel :: Label
initLabel = []

data Tree = ELeaf Label
          | SLeaf Label
          | CNode Label Tree Tree
          | Undef Label
          deriving (Eq, Show)

type Label = [Bool]

buildTree' :: Tree -> MsgResult -> Maybe Tree
buildTree' (ELeaf _) _ = Nothing
buildTree' (SLeaf _) _ = Nothing
buildTree' (Undef lbl) r = Just $ newNode lbl r
buildTree' (CNode lbl a b) r  =
      leftNode  lbl b <$> buildTree' a r <|>
      rightNode lbl a <$> buildTree' b r

buildTreeM :: MsgResult -> Maybe Tree -> Maybe Tree
buildTreeM (Conflict _) Nothing = Just $ newCNode []
buildTreeM _            Nothing = Nothing
buildTreeM res (Just t) = buildTree' t res

buildTree :: MsgResult -> Maybe Tree -> Maybe Tree
buildTree r mt = do
    t <- buildTreeM r mt
    guard . not $ isResolved' t
    return t

updateTree :: MsgResult -> Maybe Tree -> Maybe Tree
updateTree = buildTree

treeFromResults :: [MsgResult] -> Maybe Tree
treeFromResults = foldl (flip buildTree) Nothing

newNode :: Label -> MsgResult -> Tree
newNode lbl (Success _)  = SLeaf lbl
newNode lbl Empty        = ELeaf lbl
newNode lbl (Conflict _) = newCNode lbl

newCNode :: Label -> Tree
newCNode lbl = CNode lbl (Undef (decideTransmit True lbl))
                         (Undef (decideTransmit False lbl))

-- Build tree, last arg is a leftNode
leftNode :: Label -> Tree -> Tree -> Tree
leftNode lbl b a = CNode lbl a b

-- Build tree, last arg is rightNode
rightNode :: Label -> Tree -> Tree -> Tree
rightNode = CNode

-- A conflict is resolved if there is no more
-- UNDEF leaves. It can be proven by induction
-- on a structure of a tree that
-- buildTree t * == Nothing <=> there is no more UNDEFS
-- => tree is resolved
isResolved' :: Tree -> Bool
isResolved' t = isNothing $ buildTree' t (Success undefined)

isResolved :: Maybe Tree -> Bool
isResolved = isNothing

leftUndef :: Tree -> Maybe Tree
leftUndef u@(Undef _) = Just u
leftUndef (CNode _ a b) =
    leftUndef a <|> leftUndef b
leftUndef _ = Nothing

canTransmit :: Label -> Maybe Tree -> Bool
canTransmit _ Nothing = True
canTransmit lbl (Just t) =
    case leftUndef t of
      Nothing -> error "tree is resolved, shouldn't be called" -- True
      Just (Undef lbl') -> lbl' == lbl
      Just _ -> error "leftUndef is not undef, ACHTUNG!"

-- after conflict, user decides whether to
-- transmit in next slot or not. Just append decision to
-- the label.
-- XXX: shouldn't be called if no conflict with
-- user happened just before.
decideTransmit :: Bool -> Label -> Label
decideTransmit = (:)

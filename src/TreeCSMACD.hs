module TreeCSMACD
    ( Tree(..) -- NOTE: constructors are exported for GiaStation
    , Label
    , initLabel
    , initTree
    , canTransmit
    , decideTransmit
    , updateTree
    , updateTreeS
    , updateLabel
    , isResolved

    -- GiaStation
    , leftUndef
    , getNodeFromLabel
    -- testing
    ,treeFromResults
    ,treeFromResults'
    ) where

import Control.Applicative
import Control.Monad( guard )

import Data.Maybe( isNothing )

import Interface( MsgResult(..) )

initTree :: Maybe (Tree a)
initTree = Nothing

initLabel :: Label
initLabel = []

data Tree a = ELeaf Label a
            | SLeaf Label a
            | CNode Label a (Tree a) (Tree a)
            | Undef Label a
            deriving (Eq, Show)

type Label = [Bool]

buildTree' :: a -> Tree a -> MsgResult -> Maybe (Tree a)
buildTree' _ (ELeaf _ _) _ = Nothing
buildTree' _ (SLeaf _ _) _ = Nothing
buildTree' p (Undef lbl _) r = Just $ newNode lbl p r
buildTree' p (CNode lbl o a b) r  =
      leftNode  lbl o b <$> buildTree' p a r <|>
      rightNode lbl o a <$> buildTree' p b r

buildTreeM :: MsgResult -> a -> Maybe (Tree a) -> Maybe (Tree a)
buildTreeM (Conflict _) a Nothing = Just $ newCNode [] a
buildTreeM _            _ Nothing = Nothing
buildTreeM res a (Just t) = buildTree' a t res

buildTree :: MsgResult -> a -> Maybe (Tree a) -> Maybe (Tree a)
buildTree r a mt = do
    t <- buildTreeM r a mt
    guard . not $ isResolved' a t
    return t

buildTreeS :: MsgResult -> Maybe (Tree ()) -> Maybe (Tree ())
buildTreeS r = buildTree r ()

updateTreeS :: MsgResult -> Maybe (Tree ()) -> Maybe (Tree ())
updateTreeS = buildTreeS

updateTree :: MsgResult -> a -> Maybe (Tree a) -> Maybe (Tree a)
updateTree = buildTree

treeFromResults :: [MsgResult] -> Maybe (Tree ())
treeFromResults = foldl (flip buildTreeS) Nothing

treeFromResults' :: [MsgResult] -> Maybe (Tree MsgResult)
treeFromResults' = foldl buildT Nothing
    where buildT mt r = buildTree r r mt

newNode :: Label -> a -> MsgResult -> Tree a
newNode lbl a (Success _)  = SLeaf lbl a
newNode lbl a Empty        = ELeaf lbl a
newNode lbl a (Conflict _) = newCNode lbl a

newCNode :: Label -> a -> Tree a
newCNode lbl a = CNode lbl a (Undef (decideTransmit True  lbl) a)
                             (Undef (decideTransmit False lbl) a)

-- Build tree, last arg is a leftNode
leftNode :: Label -> a -> Tree a -> Tree a -> Tree a
leftNode lbl p b a = CNode lbl p a b

-- Build tree, last arg is rightNode
rightNode :: Label -> a -> Tree a -> Tree a -> Tree a
rightNode = CNode

-- A conflict is resolved if there is no more
-- UNDEF leaves. It can be proven by induction
-- on a structure of a tree that
-- buildTree t * == Nothing <=> there is no more UNDEFS
-- => tree is resolved
isResolved' :: a -> Tree a -> Bool
isResolved' a t = isNothing $ buildTree' a t (Success undefined)

isResolved :: Maybe (Tree a) -> Bool
isResolved = isNothing

leftUndef :: Tree a -> Maybe (Tree a)
leftUndef u@(Undef _ _) = Just u
leftUndef (CNode _ _ a b) =
    leftUndef a <|> leftUndef b
leftUndef _ = Nothing

-- need to reset label after successful transmission
updateLabel :: Maybe (Tree a) -> Label -> Label
updateLabel Nothing _ = []
updateLabel _ lbl = lbl

getNodeFromLabel :: Label -> Tree a -> Maybe (Tree a)
getNodeFromLabel lbl = getNodeFromPath $ reverse lbl

getNodeFromPath :: Label -> Tree a -> Maybe (Tree a)
getNodeFromPath [] t = Just t
getNodeFromPath (True:ls)  (CNode _ _ l _r) = getNodeFromPath ls l
getNodeFromPath (False:ls) (CNode _ _ _l r) = getNodeFromPath ls r
getNodeFromPath _ _ = Nothing

canTransmit :: Label -> Maybe (Tree a) -> Bool
canTransmit _ Nothing = True
canTransmit lbl (Just t) =
    case leftUndef t of
      Nothing -> error "tree is resolved, shouldn't be called" -- True
      Just (Undef lbl' _) -> lbl' == lbl
      Just _ -> error "leftUndef is not undef, ACHTUNG!"

-- after conflict, user decides whether to
-- transmit in next slot or not. Just append decision to
-- the label.
-- XXX: shouldn't be called if no conflict with
-- user happened just before.
decideTransmit :: Bool -> Label -> Label
decideTransmit = (:)

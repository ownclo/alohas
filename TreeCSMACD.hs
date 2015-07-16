module TreeCSMACD
    ( CTree
    , Label
    , initLabel
    , initCTree
    , canTransmit
    , decideTransmit
    , updateCTree
    -- testing
    ,treeFromResults
    ) where

import Control.Applicative
import Control.Arrow( first )
import Control.Monad( guard )

import Data.Maybe( isNothing )

import Interface( MsgResult(..) )

initCTree :: Maybe CTree
initCTree = Nothing

initLabel :: Tree
initLabel = Undef 0

data Tree = ELeaf Int
          | SLeaf Int
          | CNode Int Tree Tree
          | Undef Int
          deriving (Eq, Show)

type Size = Int
type CTree = (Tree, Size)
type Label = Tree

buildTree' :: Int -> Tree -> MsgResult -> Maybe CTree
buildTree' _ (ELeaf _) _ = Nothing
buildTree' _ (SLeaf _) _ = Nothing
buildTree' s (Undef _) r = Just (newNode (s+1) r, s+1)
buildTree' s (CNode i a b) r  =
      first (leftNode  i b) <$> buildTree' s a r <|>
      first (rightNode i a) <$> buildTree' s b r

buildTreeM :: MsgResult -> Maybe CTree -> Maybe CTree
buildTreeM Conflict Nothing = Just (newCNode 0, 0)
buildTreeM _        Nothing = Nothing
buildTreeM res (Just (t,s)) = buildTree' s t res

buildTree :: MsgResult -> Maybe CTree -> Maybe CTree
buildTree r mt = do
    (t, s) <- buildTreeM r mt
    guard . not $ isResolved t
    return (t, s)

updateCTree :: MsgResult -> Maybe CTree -> Maybe CTree
updateCTree = buildTree

treeFromResults :: [MsgResult] -> Maybe CTree
treeFromResults = foldl (flip buildTree) Nothing

newNode :: Int -> MsgResult -> Tree
newNode i Success  = SLeaf i
newNode i Empty    = ELeaf i
newNode i Conflict = newCNode i

newCNode :: Int -> Tree
newCNode i = CNode i (Undef (i+1)) (Undef (i+2))

leftNode :: Int -> Tree -> Tree -> Tree
leftNode i b a = CNode i a b

rightNode :: Int -> Tree -> Tree -> Tree
rightNode = CNode

-- A conflict is resolved if there is no more
-- UNDEF leaves. It can be proven by induction
-- on a structure of a tree that
-- buildTree t * == Nothing <=> there is no more UNDEFS
-- => tree is resolved
isResolved :: Tree -> Bool
isResolved t = isNothing $ buildTree' 0 t Success

leftUndef :: Tree -> Maybe Tree
leftUndef u@(Undef _) = Just u
leftUndef (CNode _ a b) =
    leftUndef a <|> leftUndef b
leftUndef _ = Nothing

canTransmit :: Label -> Maybe CTree -> Bool
canTransmit _ Nothing = True
canTransmit u@(Undef _) (Just (t, _)) =
    case leftUndef t of
      Nothing -> True  -- tree is resolved, shouldn't be called
      Just u' -> u' == u
canTransmit notUndef _ = error $ "canTransmit wrong arg: " ++ show notUndef

-- after conflict, user decides whether to
-- transmit in next slot or not.
-- XXX: shouldn't be called if no conflict with
-- user happened just before.
decideTransmit :: Bool -> CTree -> Tree
decideTransmit True  (_, i) = Undef (i+1)
decideTransmit False (_, i) = Undef (i+2)

{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
  Roshaan Quyum
  November 21, 2021
-}
module Assign_3 where

--- NOT CODED BY ME ---

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(0,1),(0,2),(2,2),(2,1)]
  in Graph nodes edges
  
--- MY CODE BEGINS --- 

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
 -    returns the greatest nodeID within a graph
 -}

maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph [] _) = Nothing
maxNodeID (Graph [Node a _] _) = Just a
maxNodeID (Graph [Node a _, Node c _] _)
  | a >= c     = Just a
  | otherwise  = Just c
maxNodeID (Graph  (Node a b : Node c d : nodes) list)
  | a >= c     = maxNodeID (Graph (Node a b : nodes) list)
  | otherwise  = maxNodeID (Graph (Node c d : nodes) list)


{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
 -    returns a graph with a new node added with a nodeID one greater than
 -     the previous greatest nodeID in the nodes, and with the inputted value
 -     v
 -
 -    getLargestID checks the nodes of an inputted graph for the greatest nodeID
 -     and then returns it. It is maxNodeID made without the Maybe data type and
 -     refigured so insertNode can use it.
 -}

getLargestID :: Graph a -> NodeID
getLargestID (Graph [] _) = 0
getLargestID (Graph [Node a _] _) = a
getLargestID (Graph [Node a _, Node b _] _) 
  | a >= b    = a
  | otherwise = b
getLargestID (Graph (Node a b : Node c d : nodes) list)
  | a >= c    = getLargestID (Graph (Node a b : nodes) list)
  | otherwise = getLargestID (Graph (Node c d : nodes) list)

insertNode :: a -> Graph a -> Graph a
insertNode v (Graph [] list) = Graph [Node 0 v] list
insertNode v (Graph nodes list) = Graph (nodes ++ [Node newID v]) list
  where 
    newID = getLargestID (Graph nodes list) + 1


{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
 -    Returns a graph with any nodes and edges corresponding to the 
 -     inputted nodeID removed from the graph
 -    
 -    removeNode' searches for a node within a list of nodes and 
 -     removes any nodes from the list whose nodeID matches with
 -     the inputted nodeID
 -   
 -    removeEdge searches for an edge within a list of edges
 -     and removes any edges whose components match with 
 -     inputted nodeID
 -}

removeNode' :: NodeID -> [Node a] -> [Node a]
removeNode' _ [] = [] 
removeNode' nID (Node a b : nodes) 
  | nID == a  = removeNode' nID nodes
  | otherwise = [Node a b] ++ removeNode' nID nodes

removeEdge :: NodeID -> [Edge] -> [Edge]
removeEdge _ [] = []
removeEdge nID (edge1 : edges) 
  | fstCheck || sndCheck = removeEdge nID edges
  | otherwise            = [edge1] ++ removeEdge nID edges
    where
      fstCheck = fst edge1 == nID
      sndCheck = snd edge1 == nID

removeNode :: NodeID -> Graph a -> Graph a
removeNode _ (Graph [] list) = Graph [] list
removeNode nID (Graph (Node a b : nodes) (edge1 : edges)) = Graph nodeRemover edgeRemover
  where
    nodeRemover = removeNode' nID (Node a b : nodes)
    edgeRemover = removeEdge nID (edge1 : edges)


{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
 -    Searches for a node within a graph which matches the nodeID of 
 -     the given nodeID. If the node is found, it is wrapped in the 
 -     Maybe data type and returned. If not found, Nothing is returned
 -}
 
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode _ (Graph [] _) = Nothing
lookupNode nID (Graph (Node a b : nodes) list)
  | nID == a   = Just (Node a b)
  | otherwise  = lookupNode nID (Graph nodes list)

{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
 -   insertEdge takes an edge and a graph as arguments and returns 
 -    a graph wrapped in the Maybe data type with the inputted edge
 -    appended to the end of the list of edges to the inputted graph
 -
 -   The nodeElem function is an implementation of the prelude function 
 -    elem over a list of nodes. nodeElem checks whether the inputted
 -    nodeID is in a given list of nodes. 
 -
 -   The containsEdge' function checks whether an edge is in a list of
 -    edges (checks if a tuple is in a list of tuples)
 -}

nodeElem :: NodeID -> [Node a] -> Bool
nodeElem _ [] = False 
nodeElem a (Node b c : nodes) 
  | a == b = True
  | otherwise = nodeElem a nodes

containsEdge' :: (NodeID, NodeID) -> [Edge] -> Bool
containsEdge' (_,_) [] = False 
containsEdge' (a,b) (edge1 : edges)
  | (a,b) == edge1 = True
  | otherwise = containsEdge' (a,b) edges

insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph nodes edges)
  | not containsBothNodes = Nothing
  | containsEdge          = Just (Graph nodes edges)
  | otherwise             = Just (Graph nodes (edges ++ [(n1,n2)]))
  where
    containsBothNodes :: Bool
    containsBothNodes = nodeElem n1 nodes && nodeElem n2 nodes
    containsEdge :: Bool
    containsEdge = containsEdge' (n1,n2) edges


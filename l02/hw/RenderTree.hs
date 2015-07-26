{-# LANGUAGE FlexibleContexts #-}
module Main where

{- The renderNodeTree function was stolen from
 - http://stackoverflow.com/a/30534198/805362
 -}

import Log
import LogAnalysis

import qualified Data.Tree as DT
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree (renderTree,symmLayout',_slHSep,_slVSep)
import Diagrams.Backend.SVG (SVG)
import Diagrams.Backend.SVG.CmdLine (defaultMain)


logs = parse "I 6 Completed armadillo processing\nI 1 Nothing to report\nI 4 Everything normal\nI 11 Initiating self-destruct sequence\nE 70 3 Way too many pickles\nE 65 8 Bad pickle-flange interaction detected\nW 5 Flange is due for a check-up\nI 7 Out for lunch, back in two time steps\nE 20 2 Too many pickles\nI 9 Back from lunch\nE 99 10 Flange failed!"


messageList = inOrder $ insertList logs Leaf

messageTree  = insertList messageList Leaf
messageTree' = balanceSubTree messageTree
messageTree'' = balanceTree messageTree


toRoseTree :: MessageTree -> DT.Tree String
toRoseTree (Node l (LogMessage _ _ m) r)
    = DT.Node m $ left ++ right
    where left  = if l /= Leaf
                  then [toRoseTree l]
                  else []
          right = if r /= Leaf
                  then [toRoseTree r]
                  else []
toRoseTree n = error ("toRoseTree: " ++ (show n))


renderNodeTree :: DT.Tree String -> QDiagram SVG V2 Double Any
renderNodeTree nodeTree = renderTree
    (\a -> letter a `atop` rect 7 1 # fc white)
    (~~)
    (symmLayout' (with{ _slHSep = 8,  _slVSep = 3}) nodeTree)
  where
     letter a = text a # font "monospace" # fontSize (local 0.3)


main :: IO ()
main = defaultMain (renderNodeTree $ toRoseTree messageTree'')

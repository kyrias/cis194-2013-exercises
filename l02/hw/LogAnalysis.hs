{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import qualified Data.List as DL
import Text.Read
import Log


-- Exercise 1
--
parseMessage :: String -> LogMessage
parseMessage xs =
    case lvl of
    Nothing   -> Unknown xs
    (Just mt) -> case ts of
                 Nothing  -> Unknown xs
                 (Just t) -> LogMessage mt t str
    where w     = words xs
          wLvl  = if head xs == 'E'
                  then drop 2 w
                  else drop 1 w
          lvl   = parseLevel w
          ts    = readMaybe (head wLvl) :: Maybe Int
          str   = unwords (drop 1 wLvl)


parseLevel :: [String] -> Maybe MessageType
parseLevel (x:xs)
    | x == "I" = Just Info
    | x == "W" = Just Warning
    | x == "E" = case severity of
                 (Just s) -> Just $ Error s
                 Nothing  -> Nothing
    where severity = readMaybe (head xs) :: Maybe Int
parseLevel _ = Nothing


parse :: String -> [LogMessage]
parse x = map parseMessage $ lines x



-- Exercise 2
--
insert :: LogMessage -> MessageTree -> MessageTree
insert newMesg Leaf = Node Leaf newMesg Leaf
insert newMesg@(LogMessage _ newTs _)  (Node l oldMesg@(LogMessage _ oldTs _) r)
    | newTs < oldTs = Node (insert newMesg l) oldMesg r
    | otherwise     = Node l oldMesg (insert newMesg r)
insert _ tree = tree

-- Apparently I pretty much did ex 3 on my own, only in a more general form..
insertList :: [LogMessage] -> MessageTree -> MessageTree
insertList []     mt = mt
insertList (l:ls) mt = insertList ls $ insert l mt


ioInsertList :: IO [LogMessage] -> IO MessageTree
ioInsertList ls = do logs <- ls
                     return $ insertList logs Leaf


-- Exercise 4
--
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt msg rt) = inOrder lt ++ msg : inOrder rt

ioInOrder :: IO MessageTree -> IO [LogMessage]
ioInOrder mt = do tree <- mt
                  return $ inOrder tree


-- Exercise 5
--
filterError :: LogMessage -> Int -> Maybe LogMessage
filterError l@(LogMessage (Error s) _ _) severity
    | s >= severity   = Just l
    | otherwise       = Nothing
filterError _ _       = Nothing

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong []   = []
whatWentWrong logs =
    whatWentWrongHelper ls
    where ls  = inOrder $ insertList logs Leaf

whatWentWrongHelper :: [LogMessage] -> [String]
whatWentWrongHelper []     = []
whatWentWrongHelper (l:ls) =
    case message of
    (Just (LogMessage _ _ s)) -> s : whatWentWrongHelper ls
    _                         -> whatWentWrongHelper ls
    where message = filterError l 50


---
--
(!!!) :: IO [a] -> Int -> IO a
(!!!) x i = do xs <- x
               return $ xs !! i


balanceSubTree :: MessageTree -> MessageTree
balanceSubTree mt = insertList messages' initialTree
    where messages    = inOrder mt
          middle      = messages !! (length messages `div` 2)
          messages'   = DL.delete middle messages
          initialTree = insert middle Leaf


balanceTree :: MessageTree -> MessageTree
balanceTree Leaf = Leaf
balanceTree tree = Node (balanceTree l) m (balanceTree r)
    where (Node l m r) = balanceSubTree tree

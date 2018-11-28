{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage m = case words m of
                  ("I":t:s) ->    LogMessage
                                    Info
                                    (read t :: Int)
                                    (unwords s :: String)
                  ("W":t:s) ->    LogMessage
                                    Warning
                                    (read t :: Int)
                                    (unwords s :: String)
                  ("E":i:t:s) ->  LogMessage
                                    (Error (read i :: Int))
                                    (read t :: Int)
                                    (unwords s :: String)
                  s ->            Unknown (unwords s :: String)


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (LogMessage nc nt ns) (Node left (LogMessage c t s) right)
  | nt < t && left == Leaf   = Node
                                  (Node Leaf (LogMessage nc nt ns) Leaf)
                                  (LogMessage c t s)
                                  right
  | nt > t && right == Leaf   = Node
                                  left
                                  (LogMessage c t s)
                                  (Node Leaf (LogMessage nc nt ns) Leaf)
  | nt < t                    = Node
                                  (insert (LogMessage nc nt ns) left)
                                  (LogMessage c t s)
                                  right
  | nt > t                    = Node
                                  left
                                  (LogMessage c t s)
                                  (insert (LogMessage nc nt ns) right)
insert lm Leaf = Node Leaf lm Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) =
  inOrder left ++ [logMessage] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (LogMessage (Error a) _ s : xs)
  | a > 50 = s : whatWentWrong xs
whatWentWrong (_:xs) = whatWentWrong xs

module LogAnalysis where
  
import Log
import Data.List.Split
import Text.Read

-- Ex1

parseMessageType :: String -> (Maybe MessageType, String)
parseMessageType "" = (Nothing, "")
parseMessageType s =
  let 
    (first:second:rest) = words s
  in
    case first of
      "I" -> (Just Info, unwords $ second: rest)
      "W" -> (Just Warning, unwords $ second: rest)
      "E" -> (Just $ Error ((read second) :: Int), unwords rest)
      _ -> (Nothing, unwords rest)
      
  
parseTimeStamp :: String -> (TimeStamp, String)
parseTimeStamp s =
  let
    (first:rest) = words s
  in
    ((read first) :: TimeStamp, unwords rest)
    
parseMessage :: String -> LogMessage
parseMessage s =
  let 
    (messageType, rest) = parseMessageType s
    (timeStamp, messageString) = parseTimeStamp rest
  in
    case messageType of
      Nothing -> Unknown s
      Just x -> LogMessage x timeStamp messageString
      

parse :: String -> [LogMessage]
parse s = map parseMessage $ splitOn "\n" s


-- Ex2
-- Insert a new @LogMessage@ into an existing MessageTree, producing a new MessageTree.
-- Insert may assume that it is given a sorted MessageTree, and must produce a
-- new sorted MessageTree containing the new LogMessage in addition to the
-- contents of the original MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = (Node Leaf m Leaf)
insert m (Node left message right)
  | timeStamp1 < timeStamp2 = (Node (insert m left) message right)
  | timeStamp1 >= timeStamp2 = (Node left message (insert m right))
  where 
    (LogMessage _ timeStamp1 _) = m
    (LogMessage _ timeStamp2 _) = message
    

-- Ex3
-- Build a complete MessageTree from a list of messages.
build :: [LogMessage] -> MessageTree
build [] = Leaf
build logList = insert (head logList) $ build $ tail logList


-- Ex4
-- Takes a sorted MessageTree and produces a list of all the LogMessages it 
-- contains, sorted by timestamp from smallest to biggest.
-- (This is known as an in-order traversal of the MessageTree.)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right


-- Ex5
-- Takes an unsorted list of LogMessages, and return a list of the 
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logList = map extractMessageString $ filter isErrorMessage $ inOrder $ build logList


isErrorMessage :: LogMessage -> Bool
isErrorMessage (LogMessage (Error severity) _ _)
  | severity >= 50 = True
  | otherwise = False
isErrorMessage _ = False
  
extractMessageString :: LogMessage -> String
extractMessageString (LogMessage _ _ messageString) = messageString 

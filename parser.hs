module Parser where

import Data.Char
import Data.List
import Board

score :: String -> Either String (Int, Int)
score list = case parsing of
  Left parsing -> Left parsing
  Right parsing -> case (checkGameCorrectness parsing) of
                  False -> Left "Game is not correct (shot was made two times into one field)"
                  True -> Right (countScore parsing)
  where parsing = parseAllMessages list

data BattleshipMessage = BattleshipMessage {
  xy :: (Char,Int)
, result :: String
} deriving Show

parseAllMessages :: String -> Either String [BattleshipMessage]
parseAllMessages (']':restt) = Right []
parseAllMessages msg = case parsedMessage of
  Left parsedMessage -> Left parsedMessage
  Right parsedMessage -> Right (fst parsedMessage)
  where
    parseAllMessages' :: Either String ([BattleshipMessage], String) -> Either String ([BattleshipMessage], String)
    parseAllMessages' (Left errmsg) = Left errmsg
    parseAllMessages' (Right (array, rest))
      | head rest == ']' = Right (reverse array, "")
      | otherwise =
      let
      parsedMsg = parseMessage rest
      answer = case parsedMsg of
        Left parsedMsg -> Left parsedMsg
        Right parsedMsg -> Right ((fst parsedMsg):array, snd parsedMsg)
      in parseAllMessages' answer
    parsedMessage = parseAllMessages' (Right ([],msg))


parseMessage :: String -> Either String (BattleshipMessage, String)
parseMessage ('[':'"':'c':'o':'o':'r':'d':'"':',':msg) =
  case parseCoord of
    Left parseCoord -> Left parseCoord
    Right parseCoord -> let x = (snd parseCoord) in case x of
      ('"':'r':'e':'s':'u':'l':'t':'"':',':msg) -> let y = parseResult msg in case y of
        Left y -> Left y
        Right y -> let z = (snd y) in case z of
          ('"':'p':'r':'e':'v':'"':',':'n':'u':'l':'l':msg) -> Right (BattleshipMessage {xy = (fst parseCoord), result = fst y}, msg)
          ('"':'p':'r':'e':'v':'"':',':msg) -> Right (BattleshipMessage {xy = (fst parseCoord), result = fst y},msg)
      _ -> Left "Result keyword not found"
    where parseCoord = parseCoordinates msg
parseMessage _ = Left "Bad format"

parseCoordinates :: String -> Either String ((Char,Int),String)
parseCoordinates ('[':']':',':rest) = Right (('Z',0), rest)
parseCoordinates ('[':'"':a:'"':',':'"':b:'"':']':',':rest) = Right ((a, digitToInt b),rest)
parseCoordinates ('[':'"':a:'"':',':'"':b:c:'"':']':',':rest) = Right ((a, ((digitToInt b * 10) + (digitToInt c))),rest)
parseCoordinates _ = Left "Unknown coordinates type"

parseResult :: String -> Either String (String, String)
parseResult ('n':'u':'l':'l':',':msg) = Right ("null", msg)
parseResult ('"':'M':'I':'S':'S':'"':',':msg) = Right ("MISS", msg)
parseResult ('"':'H':'I':'T':'"':',':msg) = Right ("HIT", msg)
parseResult _ = Left "Unknown result type"


countScore :: [BattleshipMessage] -> (Int,Int)
countScore list = (countHits (everySecondEven list), countHits (everySecondOdd list))

countHits :: [BattleshipMessage] -> Int
countHits x = countHits' x 0
  where
    countHits' [] acc = acc
    countHits' (x:xs) acc = if (result x == "HIT") then countHits' xs (acc+1)
                            else countHits' xs acc

checkGameCorrectness :: [BattleshipMessage] -> Bool
checkGameCorrectness list = allDifferent (recordToCoordTuple (everySecondEven list)) && allDifferent (recordToCoordTuple (everySecondOdd list))

allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
  []      -> True
  (x:xs)  -> x `notElem` xs && allDifferent xs

recordToCoordTuple :: [BattleshipMessage] -> [(Char,Int)]
recordToCoordTuple list = case list of
  [] -> []
  (x:xs) -> xy x : recordToCoordTuple xs

everySecondEven :: [BattleshipMessage] -> [BattleshipMessage]
everySecondEven list = case list of
  [] -> []
  (x:xs) -> if even (length list) then x : everySecondEven xs
                                  else everySecondEven xs

everySecondOdd :: [BattleshipMessage] -> [BattleshipMessage]
everySecondOdd list = case list of
  [] -> []
  (x:xs) -> if odd (length list) then x : everySecondOdd xs
                                  else everySecondOdd xs

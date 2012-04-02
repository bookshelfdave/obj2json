module ObjParser where

import Data.Char
import Data.Maybe

-- raw types
data RawType =
		O String |
		V Float Float Float  |
		VN Float Float Float  |
		F [Integer] |
		USEMATL String |
		S String |
		MTLLIB String
		deriving (Show,Read)

firstToken :: String -> Maybe String
firstToken [] = Nothing
firstToken l = Just h
				where h = head $ words l

isCommentLine :: Maybe String -> Bool
isCommentLine Nothing = True
isCommentLine (Just l) = c == '#'
				  where c = head l

getDataLines :: String -> [String]
getDataLines contents = filter (not . isCommentLine . firstToken) $ lines contents
			

_processLine :: String -> String -> RawType
_processLine _ line = read line :: RawType

processLine :: String -> RawType
processLine l = 
			_processLine ft l
			where 
				ft = map toUpper $ (fromJust $ firstToken l)

processData :: String -> IO ()
processData contents = 
					return ()
				where 
					ls = getDataLines contents
					mapped = map processLine ls
			  		
main :: IO ()
main = do
		contents <- readFile "/Users/dparfitt/src/cube.obj"
		processData contents		
		return ()

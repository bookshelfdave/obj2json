--module Main (main,getDataLines,processLine) where
-- Currently only supports one object per file.
-- partition lists based on line type = o and then call process data on
-- each segment

import Data.Char
import Data.Maybe
import Control.Monad
import Text.Printf

-- raw types
data RawType =
	O String |
	V Double Double Double  |
	VN Double Double Double |
	F [Int] |		
	USEMTL String |
	S String |
	MTLLIB String |
	Unknown String 
	deriving (Read)


instance Show RawType where  
	show (O s) = "Object"
	show (V x y z) = printf "[%f,%f,%f]" x y z
	show (VN x y z) = printf "[%f,%f,%f]" x y z
	show (F fs) = show fs
	show (USEMTL s) = "" 
	show (S s) = ""
	show (MTLLIB s) = ""
	show (Unknown s) = ""

-- maybe I'm missing a haskell-ism here
isObj :: RawType -> Bool
isObj (O xs) = True
isObj _ = False

isVert :: RawType -> Bool
isVert (V x y z) = True
isVert _ = False

isVertNorm :: RawType -> Bool
isVertNorm (VN x y z) = True
isVertNorm _ = False

isFace :: RawType -> Bool
isFace (F xs) = True
isFace _ = False



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
			

getStringData :: String -> String
getStringData s = (words s) !! 1

-- abust of pattern matching? you decide :-)
getData :: String -> String -> RawType
getData "V" line = V (nums !! 0) (nums !! 1) (nums !! 2)
					where 
						numStrings = tail $ words line
						nums = map (\x -> read x :: Double) numStrings

-- warning: repeated code!
getData "VN" line = VN (nums !! 0) (nums !! 1) (nums !! 2)
					where 
						numStrings = tail $ words line
						nums = map (\x -> read x :: Double) numStrings
getData "F" line = F numData
					where 
						strData = words $ tail line
						numData = map (\n -> read n :: Int) strData

getData "O" line = O objname
						where objname = getStringData line						
getData "USEMTL" line = USEMTL matlName
							where matlName = getStringData line
getData "S" line = S s 
						where s = getStringData line

getData "MTLLIB" line = MTLLIB m
						where m = getStringData line
getData _ line = Unknown line

processLine :: String -> RawType
processLine l = 
			getData ft l
			where 
				ft = map toUpper $ (fromJust $ firstToken l)

processData :: String -> IO ()
processData contents = 				
					--mapM_ putStrLn mappedStrs >>					
						outputObject mapped
				where 
					ls = getDataLines contents
					mapped = map processLine ls
					mappedStrs = map show mapped

outputObject :: [RawType] -> IO ()
outputObject objData = do
						putStrLn $ show verts
						putStrLn $ show vertNorms
						putStrLn $ show faces
					   where 
					   	verts = filter isVert objData
					   	vertNorms = filter isVertNorm objData
					   	faces = filter isFace objData					   	

objsToJson :: [RawType] -> IO ()
objsToJson o = do
				putStrLn "{\"objects\":"
				putStrLn "[]"
				putStrLn "}"

			  		
main :: IO ()
main = do
		contents <- readFile "cube.obj"
		processData contents		
		return ()


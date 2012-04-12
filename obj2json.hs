-- TODO: Currently only supports one object per file.

import Data.Char
import Data.Maybe
import Data.List
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
        show (O s) = s
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

-- renumber Face vert references starting at 0
getData "F" line = F adjustedNumData
                   where 
                   strData = words $ tail line
                   numData = map (\n -> read n :: Int) strData
                   adjustedNumData = map (\n -> n - 1) numData

getData "O" line = O objname
                   where objname = getStringData line                                              

getData "USEMTL" line = USEMTL matlName
                        where matlName = getStringData line
getData "S" line = S s 
                   where s = getStringData line

getData "MTLLIB" line = MTLLIB m
                        where m = getStringData line

getData _ line = Unknown line


-- produces "\"Foo\"" from "Foo"
jsonStr :: String -> String
jsonStr s = "\"" ++ s ++ "\""


jsonKeyValQuote :: String -> String -> String
jsonKeyValQuote k v = jsonStr k ++ ":" ++ jsonStr v

jsonKeyVal :: String -> String -> String
jsonKeyVal k v = jsonStr k ++ ":" ++ v


outputObject :: [RawType] -> IO ()
outputObject objData = do
                        let nameNode = jsonKeyValQuote "name" $ show obj                                                
                        let vertsNode = jsonKeyVal "verts" $ show verts
                        let vertNormsNode = jsonKeyVal "vert_norms" $ show vertNorms
                        let facesNode = jsonKeyVal "faces" $ show faces
                        let odata = [nameNode, vertsNode, vertNormsNode, facesNode]
                        putStr "{"
                        mapM_ putStr $ intersperse "," odata
                        putStrLn "}"
                       where 
                        verts = filter isVert objData
                        vertNorms = filter isVertNorm objData
                        faces = filter isFace objData                                           
                        obj = head $ filter isObj objData                                               

outputObjects :: [RawType] -> IO ()
outputObjects o = do
                    putStr "{"                              
                    putStr $ jsonStr "objects"
                    putStr ": ["
                    -- TODO, not correct for more than one object
                    outputObject o
                    putStrLn "]}"

processLine :: String -> RawType
processLine l = getData ft l
                where 
                ft = map toUpper $ (fromJust $ firstToken l)

processData :: String -> IO ()
processData contents =                          
                --mapM_ putStrLn mappedStrs >>                                  
                outputObjects mapped
            where 
                ls = getDataLines contents
                mapped = map processLine ls
                --mappedStrs = map show mapped
                        
main :: IO ()
main = do
        contents <- getContents
        processData contents            
        return ()


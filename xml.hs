module XML (Attributes, XML(..), parseXML) where 

import Text.ParserCombinators.Parsec
import IO hiding (try)
import Control.Applicative hiding ((<|>), many)

---- Data

newtype Attributes = Attributes [(String, String)]

data XML = Element String Attributes [XML]
         | Content String
         | Comment String
         | Doctype String
         | DTD     String
         | CData   String

---- XML Printer

instance Show XML where
    show (Element tag attrs xs) = "\n<" ++ tag ++ (show attrs) ++ ">"
                                  ++ (((replace '\n' "\n    ") . concat . (map show)) xs)
                                  ++ "\n</" ++ tag ++ ">"
    show (Content content) 
        | length (strip content) == 0 = ""
        | otherwise                   = "\n" ++ content
    show (Comment comment)      = "\n<!--"      ++ comment ++ "-->"
    show (Doctype doctype)      = "\n<!DOCTYPE" ++ doctype ++ ">"
    show (CData   cdata)        = "\n<![CDATA[" ++ cdata   ++ "]]>"
    show (DTD     dtd)          = "\n<?xml"     ++ dtd     ++ "?>"

instance Show Attributes where
    show (Attributes ((name, value):xs)) = " " ++ name 
                                           ++ "=\"" ++ value 
                                           ++ "\""  ++ (show $ Attributes xs)
    show (Attributes [])                 = ""

---- XML Parser

parseXML :: Parser [XML]
parseXML  = manyTill (tag <|> content) eof
      
content   = do c <- many1 (noneOf "<>")
               return $ Content (strip c)

tag       = char '<' >> (element <|> meta)

meta      = do char '!' <|> char '?'
               doctype  <|> comment <|> dtd <|> cdata
                   where cdata   = text CData   "[CDATA[" "]]>"
                         dtd     = text DTD     "xml"     "?>"
                         comment = text Comment "--"      "-->"
                         doctype = text Doctype "DOCTYPE" ">"

element   = do (t, attrs) <- open
               inner      <- manyTill (tag <|> content) (try $ close t)
               return $ Element t attrs inner

open      = do name  <- many tagChar
               spaces
               attrs <- sepBy attribute spaces1 
               char '>'
               return (name, (Attributes attrs))

close t   = string "</" >> string t >> char '>' >> return ()

attribute = do name  <- many1 tagChar
               spaces
               char '='
               spaces
               char '\'' <|> char '"'
               value <- many1 (noneOf "'\"")
               char '\'' <|> char '"'
               return (name, value)

tagChar   = lower <|> upper <|> digit <|> (oneOf ":-")

---- Utilities

replace :: Char -> String -> String -> String
replace f r (x:xs) | x == f    = r ++ (replace f r xs)
                   | otherwise = x :  (replace f r xs)
replace _ _ []                 = ""

find :: [XML] -> String -> [String]
find ((Element tag _ xmls):xs) name 
    | tag == name = (map show xmls) ++ (find xs name)
    | otherwise   = (find xmls name) ++ (find xs name)
find (_:xs) name  = (find xs name)
find [] _         = []

spaces1 = space >> spaces

text :: (String -> XML) -> String -> String -> Parser XML
text c s e = do string s
                r <- manyTill anyChar (try $ string e)
                return $ c (strip r)

clean :: String -> String
clean = filter (`notElem` "\n\r\t")

strip :: String -> String
strip = strip' . strip'
    where strip' = reverse . (dropWhile (== ' '))

test =  "~/Desktop/cnn_topstories.xml"

extract :: String -> IO [XML]
extract file = do fileHandle <- openFile file ReadMode
                  contents   <- hGetContents fileHandle
                  return $ check $ parse parseXML file (clean contents)
                      where check (Right xs) = xs
                            check _          = undefined








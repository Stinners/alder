module Renderer

import Data.Strings

import Attributes

str : List String -> String 
str = foldl (++) ""

renderAttr : List Attribute -> String
renderAttr [] = ""
renderAttr props = " " ++ (unwords . map render) props

renderChildren : List String -> String
renderChildren children = str children

export
tag : String -> List Attribute -> List String -> String 
tag tagName propsList children =
  fastConcat ["<", tagName, (renderAttr propsList), ">", (renderChildren children), "</", tagName, ">"]

export 
voidTag : String -> List Attribute -> String
voidTag tagName propsList = 
  fastConcat ["<", tagName, (renderAttr propsList), ">"]

export 
html5 : String -> String 
html5 contents = fastConcat ["<!DOCTYPE html>", contents]

export 
comment : String -> String 
comment contents = fastConcat ["<!-- ", contents, " -->"]

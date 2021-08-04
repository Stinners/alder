module Renderer

import Data.Strings

import Attributes
import Tag


str : List String -> String 
str = foldl (++) ""

renderAttr : List Attribute -> String
renderAttr [] = ""
renderAttr props = " " ++ (unwords . map Attributes.render) props

renderChildren : List String -> String
renderChildren children = str children

close : String -> String 
close name = fastConcat ["</", name, ">"]

open' : String -> List Attribute -> String 
open' name attr = fastConcat ["<", name, renderAttr attr, ">"]

mutual
  renderChildren' : List VNode -> String 
  renderChildren' =  str . (map render)

  export
  render : VNode -> String
  render (Tag name attr children) = fastConcat [open' name attr, renderChildren' children, close name]
  render (VoidTag name attr) = open' name attr
  render (Text text) = text


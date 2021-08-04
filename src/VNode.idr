module VNode

import Attributes

public export
data VNode = Tag String (List Attribute) (List VNode)
           | VoidTag String (List Attribute)
           | Text String

str : List String -> String 
str = foldl (++) ""

close : String -> String 
close name = fastConcat ["</", name, ">"]

open' : String -> List Attribute -> String 
open' name attr = fastConcat ["<", name, show attr, ">"]

mutual
    renderChildren : List VNode -> String
    renderChildren children = str (map render children)
    
    render : VNode -> String
    render (Tag name attr children) = fastConcat [open' name attr, renderChildren children, close name]
    render (VoidTag name attr) = open' name attr
    render (Text text) = text

export
Show VNode where 
  show = render

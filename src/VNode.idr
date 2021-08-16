module VNode

import Attributes

public export
NodeRef : Type 
NodeRef = AnyPtr

public export
record Metadata where 
  constructor MkMetadata
  ref  : Maybe AnyPtr

mutual 
  public export
  data VNode = Tag String (List Attribute) (List VDOM)
             | VoidTag String (List Attribute)
             | Text String

  public export
  record VDOM where 
    constructor MkVNode 
    metadata : Metadata 
    node : VNode

initMetadata : Metadata
initMetadata = MkMetadata Nothing

export
defineTag : String -> List Attribute -> List VDOM -> VDOM
defineTag tagName attr children = MkVNode initMetadata (Tag tagName attr children)

export
defineVoidTag : String -> List Attribute -> VDOM
defineVoidTag tagName attr = MkVNode initMetadata (VoidTag tagName attr)

export 
text : String -> VDOM
text string = MkVNode initMetadata (Text string)

close : String -> String 
close name = fastConcat ["</", name, ">"]

open' : String -> List Attribute -> String 
open' name attr = fastConcat ["<", name, show attr, ">"]

export
Show VDOM where
  show (MkVNode _ (Tag name attrs children)) = fastConcat [open' name attrs, (fastConcat . map show) children, close name]
  show (MkVNode _ (VoidTag name attrs)) = fastConcat [open' name attrs, close name]
  show (MkVNode _ (Text string)) = string

module VNode

import Attributes

public export
NodeRef : Type 
NodeRef = AnyPtr 

public export
data DOMState = L | V

public export
data NodeType = TagNode | VoidNode | TextNode

public export
record Metadata where 
  constructor MkMetadata

initMetadata : Metadata
initMetadata = MkMetadata

mutual
  public export
  data Node : (life:DOMState) -> NodeType -> Type where 
    Tag  : String -> List Attribute -> List (DOM life) -> Node life TagNode 
    Void : String -> List Attribute                    -> Node life VoidNode
    Text : String                                      -> Node life TextNode

  public export
  data DOM : (life:DOMState) -> Type where 
    LiveDOM : Metadata -> Node L nodeType -> NodeRef -> DOM L
    VDOM    : Metadata -> Node V nodeType ->            DOM V

export
defineTag : String -> List Attribute -> List (DOM V) -> DOM V
defineTag name attrs children = VDOM initMetadata (Tag name attrs children)

export
defineVoidTag : String -> List Attribute -> DOM V
defineVoidTag name attrs = VDOM initMetadata (Void name attrs)

export 
text : String -> DOM V
text string = VDOM initMetadata (Text string)

module AlVNode 

import Data.Strings 

import Attributes


public export
NodeRef : Type 
NodeRef = AnyPtr 

data DOMState = L | V
data NodeType = TagNode | VoidNode | TextNode

record Metadata where 
  constructor MkMetadata

mutual
  data Node : (life:DOMState) -> NodeType -> Type where 
    Tag  : String -> List Attribute -> List (DOM life) -> Node life TagNode 
    Void : String -> List Attribute                    -> Node life VoidNode
    Text : String                                      -> Node life TextNode

  data DOM : (life:DOMState) -> Type where 
    LiveDOM : Metadata -> Node L nodeType -> NodeRef -> DOM L
    VDOM    : Metadata -> Node V nodeType ->            DOM V

-- Creates an empty node an returns a pointer to it
%foreign "browser:lambda: node => document.createElement(node)"
prim__makeNode : String -> PrimIO AnyPtr

-- Returns undefined
%foreign "browser:lambda: (node, attr, value) => node.setAttribute(attr, value)"
prim__setAttr : AnyPtr -> String -> String -> PrimIO AnyPtr

%foreign "browser:lambda: (parent, child) => parent.appendChild(child)"
prim__addNode : AnyPtr -> AnyPtr -> PrimIO AnyPtr

%foreign "browser:lambda: id => document.getElementById(id)"
prim__getRoot : String -> PrimIO AnyPtr

%foreign "browser:lambda: text => document.createTextNode(text)"
prim__madeText : String -> PrimIO AnyPtr

%foreign "browser:lambda: value => console.log(value)"
prim__consoleLog : AnyPtr -> PrimIO AnyPtr

%foreign "browser:lambda: value => console.log(value)"
prim__consoleString : String -> PrimIO AnyPtr

setAttributes : NodeRef -> List Attribute -> IO NodeRef
setAttributes node attrs = do
  let _ = map (setAttribute node) attrs
  pure node
  where 
    setAttribute : NodeRef -> Attribute -> IO NodeRef
    setAttribute node (Class xs) = primIO $ prim__setAttr node "class" (unwords xs)
    setAttribute node (Accept x) = primIO $ prim__setAttr node "accept" x
    setAttribute node (AcceptCharSet x) = primIO $ prim__setAttr node "accept-charset" x
    setAttribute node (AcessKey x) = primIO $ prim__setAttr node "accesskey" x
    setAttribute node (Action x) = primIO $ prim__setAttr node "action" x
    setAttribute node Async = primIO $ prim__setAttr node "async" ""   -- TODO Fix this
    setAttribute node (Cols x) = primIO $ prim__setAttr node "cols" (show x)
    setAttribute node (Attr x y) = primIO $ prim__setAttr node x y
    setAttribute node (Id x) = primIO $ prim__setAttr node "is" x


makeNode : (Node V tagType) -> IO NodeRef
makeNode (Tag name attrs children) = do
  node <- primIO $ prim__makeNode name 
  _ <- setAttributes node attrs
  pure node 

makeNode (Void name attrs) = do
  node <- primIO $ prim__makeNode name 
  _ <- setAttributes node attrs
  pure node 

makeNode (Text text) = primIO $ prim__madeText text

attachNode : (parent : NodeRef) -> (child : NodeRef) -> IO ()
attachNode parent child = do
  _ <- primIO $ prim__addNode parent child
  pure ()

render : (root : NodeRef) -> (vdom : DOM V) -> IO (DOM L)
render root (VDOM meta node) = do
  ref <- makeNode node
  _ <-  attachNode root ref
  case node of 
    (Tag name attr children) => do 
      children <- traverse (render ref) children
      let liveNode = Tag name attr children
      pure (LiveDOM meta liveNode ref)
    (Text string) => pure (LiveDOM meta (Text string) ref)
    (Void name attr) => pure (LiveDOM meta (Void name attr) ref)

%foreign "browser:lambda: node => node.remove()"
prim__remove : NodeRef -> PrimIO ()

reRender : (root: NodeRef) -> (old : DOM L) -> (new : DOM V) -> IO (DOM L)
reRender root (LiveDOM _ _ oldRef) new = do 
  _ <- primIO $ prim__remove oldRef
  render root new

diffChildren : (root : NodeRef) -> (old : List (DOM L)) -> (new : List (DOM V)) -> IO (List (DOM L))

-- TODO make diff a composition of these functions 
-- several of them will do nothing in most cases

-- Update meta
-- Rerender All
-- Update attr 
-- Update old 

-- TODO make this handle metadata 
diff : (root : NodeRef) -> (old : DOM L) -> (new : DOM V) -> IO (DOM L)
diff root 
     old@(LiveDOM oldMeta oldNode oldRef)
     new@(VDOM newMeta newNode) = 
  case (oldNode, newNode) of 
   ((Tag oldName oldAttr oldChildren), (Tag newName newAttr newChildren)) => 
     if oldName /= newName 
        then reRender root old new
     else if True -- oldAttr /= newAttr 
        then reRender root old new  -- TODO this should also check the children 
     else do
       children <- diffChildren oldRef oldChildren newChildren 
       let liveTag = Tag newName newAttr children
       pure (LiveDOM newMeta liveTag oldRef)

   ((Void oldName oldAttr), (Void newName newAttr)) => 
     if oldName /= newName 
        then reRender root old new
     else if True -- oldAttr /= newAttr 
        then reRender root old new 
     else pure old

   ((Text oldText), (Text newText)) =>
     if oldText /= newText 
        then reRender root old new
        else pure old

   (_, _) => reRender root old new 

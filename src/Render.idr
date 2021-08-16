module Render

import Data.Strings

import VNode
import Attributes

public export 
Node : Type 
Node = AnyPtr 

-- FFI Primitives 

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


-- Utility Functions 

consoleLog : AnyPtr -> IO AnyPtr 
consoleLog value = primIO $ prim__consoleLog value

consoleString : String -> IO AnyPtr 
consoleString value = primIO $ prim__consoleString value

getRoot : String -> IO AnyPtr 
getRoot id = primIO $ prim__getRoot id


-- Adding attributes to a node 

setAttributes : Node -> List Attribute -> IO Node
setAttributes node attrs = do
  let _ = map (setAttribute node) attrs
  pure node
  where 
    setAttribute : Node -> Attribute -> IO Node
    setAttribute node (Class xs) = primIO $ prim__setAttr node "class" (unwords xs)
    setAttribute node (Accept x) = primIO $ prim__setAttr node "accept" x
    setAttribute node (AcceptCharSet x) = primIO $ prim__setAttr node "accept-charset" x
    setAttribute node (AcessKey x) = primIO $ prim__setAttr node "accesskey" x
    setAttribute node (Action x) = primIO $ prim__setAttr node "action" x
    setAttribute node Async = primIO $ prim__setAttr node "async" ""   -- TODO Fix this
    setAttribute node (Cols x) = primIO $ prim__setAttr node "cols" (show x)
    setAttribute node (Attr x y) = primIO $ prim__setAttr node x y
    setAttribute node (Id x) = primIO $ prim__setAttr node "is" x

-- Create a new node 

makeNode : VNode -> IO Node 

makeNode (Tag name attrs children) = do
  node <- primIO $ prim__makeNode name 
  _ <- setAttributes node attrs
  pure node 

makeNode (VoidTag name attrs) = do 
  node <- primIO $ prim__makeNode name 
  _ <- setAttributes node attrs
  pure node

makeNode (Text text) = primIO $ prim__madeText text

-- Render the virtual DOM 
-- Returns the VDOM, but with references to each coresponding concrete 
-- Node added to the metadata
export
renderTree : (root : Node) -> VDOM -> IO VDOM
renderTree root vdom = do
  node <- makeNode vdom.node 
  let vdom = record { metadata->ref = Just node } vdom
  case vdom.node of 
    (Tag name attrs children) => do 
      newChildren <- traverse (renderTree node) children
      let newVNode = Tag name attrs children
      pure (record { node = newVNode } vdom )
    _ => pure vdom



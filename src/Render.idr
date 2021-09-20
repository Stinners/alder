module Render

import Data.String

import VNode
import Attributes

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

%foreign "browser:lambda: node => node.innerHTML = ''"
prim__clearNode : AnyPtr -> PrimIO AnyPtr


export
getRoot : String -> IO AnyPtr
getRoot id = primIO $ prim__getRoot id

export 
consoleLog : Show a => a -> IO ()
consoleLog x = do 
  _ <- primIO $ prim__consoleString (show x) 
  pure ()

export
setAttributes : NodeRef -> List Attribute -> IO NodeRef
setAttributes node attrs = do
  _ <- traverse (setAttribute node) attrs
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
    setAttribute node (Id x) = primIO $ prim__setAttr node "id" x


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


render' : (root : NodeRef) -> (vdom : DOM V) -> IO (DOM L)
render' root (VDOM meta node) = do
  ref <- makeNode node
  _ <-  attachNode root ref
  case node of 
    (Tag name attr children) => do 
      children <- traverse (render' ref) children
      let liveNode = Tag name attr children
      pure (LiveDOM meta liveNode ref)
    (Text string) => pure (LiveDOM meta (Text string) ref)
    (Void name attr) => pure (LiveDOM meta (Void name attr) ref)

export
render : (root : NodeRef) -> (vdom : DOM V) -> IO (DOM L)
render root vdom = do 
  _ <- primIO $ prim__clearNode root 
  render' root vdom 

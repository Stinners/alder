module Main 

import HTML


{-
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

consoleLog : AnyPtr -> IO AnyPtr 
consoleLog value = primIO $ prim__consoleLog value

consoleString : String -> IO AnyPtr 
consoleString value = primIO $ prim__consoleString value

getRoot : String -> IO AnyPtr 
getRoot id = primIO $ prim__getRoot id

render : AnyPtr -> VNode -> IO AnyPtr
render root (Tag name xs ys) = do
  node  <- primIO $ prim__makeNode name
  _     <- primIO $ prim__setAttr node "class" "idris"
  child <- primIO $ prim__addNode root node
  pure child
render root (VoidTag x xs) = primIO $ prim__getRoot "test"
render root (Text text) = do
  node <- primIO $ prim__madeText text
  _ <- primIO $ prim__addNode root node
  pure node

main : IO ()
main = do
  let node = Tag "p" [] []
  let text = Text "Hello from Idris!"

  _ <- consoleString "Starting"
  root <- getRoot "idris-root"
  _ <- consoleLog root
  p <- render root node 
  _ <- render p text
  pure ()
  -}

main : IO ()
main = do
  let htmlTest = html [] 
                     [ head [] []
                     , body [] [
                         h1 [Class ["header"], Id "main-header"] [text "Hello, World!"] 
                         ,  p [] [text "This is test"]
                         ]
                     ]
  putStrLn (show htmlTest)

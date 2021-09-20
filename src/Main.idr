module Main 

import HTML
import VNode 
import Render
import Attributes

Model : Type 
Model = Integer

%foreign "browser:lambda: (node, func) => node.onclick = func"
prim__onClick : AnyPtr -> (IO ()) -> PrimIO AnyPtr 

initModel : Model 
initModel = 0

view : Model -> DOM V
view count = 
  div [] [
    h1 [] [text "Count"]
    , span [] [text count]
    , button [Id "inc", Class ["test", "foobar"]] [text "+"]
  ]

update : Model -> IO ()
update model = do
  root <- getRoot "idris-root"
  let model = model + 1 
  let html = view model
  live <- render root html
  button <- getRoot "inc"
  _ <- primIO $ prim__onClick button (update model)
  pure ()

main : IO ()
main = do
  update (-1)
  
                     

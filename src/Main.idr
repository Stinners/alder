module Main 

import HTML

main : IO ()
main = do
  let htmlTest = html [] 
                     [ head [] []
                     , body [] [
                         h1 [Class ["header"], Id "main-header"] [Text "Hello World"] 
                         ,  p [] [Text "This is a test"]
                         ]
                     ]
  putStrLn (render htmlTest)

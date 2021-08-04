module Main 

import HTML

main : IO ()
main = do
  let htmlTest = html [] 
                     [ head [] []
                     , body [] [
                         h1 [Class ["header"], Id "main-header"] ["Hello World"] 
                         ,  p [] ["This is a test"]
                         ]
                     ]
  putStrLn htmlTest

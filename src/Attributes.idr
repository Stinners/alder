
module Attributes

import Data.Strings

quote : String -> String 
quote string = fastConcat ["\"", string, "\""]

public export
data Attribute = Class (List String) 
               | Accept String 
               | AcceptCharSet String
               | AcessKey String
               | Action String 
               | Async 
               | Cols Integer
               | Attr String String
               | Id String

attr : Show ty => String -> {auto value : ty} -> String 
attr name = fastConcat [name, "=", show $ value]

export
render : Attribute -> String 
render (Class x) = "class=\"" ++ (unwords x) ++ "\""
render (Accept x) = attr "accept"
render (AcceptCharSet x) = attr "accept-char-set"
render (AcessKey x) = attr "attr accesskey"
render (Action x) = attr "action"
render (Cols x) = attr "cols" 
render (Id x) = attr "id"
render (Attr name value) = attr name {value = value}
render Async = "async"

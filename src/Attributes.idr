
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

Show Attribute where
  show (Class x) = "class=\"" ++ (unwords x) ++ "\""
  show (Accept x) = attr "accept"
  show (AcceptCharSet x) = attr "accept-char-set"
  show (AcessKey x) = attr "attr accesskey"
  show (Action x) = attr "action"
  show (Cols x) = attr "cols" 
  show (Id x) = attr "id"
  show (Attr name value) = attr name {value = value}
  show Async = "async"

export
Show (List Attribute) where
  show [] = ""
  show props = " " ++ (unwords . map show) props ++ " "

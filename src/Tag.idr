module Tag

import Attributes

public export
data VNode = Tag String (List Attribute) (List VNode)
           | VoidTag String (List Attribute)
           | Text String


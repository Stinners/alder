module Diff 

import VNode 
import Render

%foreign "browser:lmabda: node => node.remove()"
prim__remove : Node -> PrimIO ()

%foreign "browser:lambda: node => node.parentElement"
prim__getParent : Node -> PrimIO Node

updateTree : VDOM -> VDOM -> IO VDOM
updateTree old new = 
  case old.metadata.ref of 
      Nothing => pure new     -- handle errors properly here 
      Just ptr => do
        parent <- primIO $ prim__getParent ptr
        _ <- primIO $ prim__remove ptr
        renderTree parent new

updateNode : Node -> VNode -> VNode 

diff : VDOM -> VDOM -> IO VDOM
diff oldDOM newDOM =
  case (oldDOM.node, newDOM.node) of 
      ((Tag name attr _), (Tag newName newAttr _)) => ?case1 
      ((VoidTag _ _), (VoidTag _ _)) => updateNode 
      ((Text text), (Text newText)) => ?case3 
      (_, _) => updateTree oldDOM newDOM

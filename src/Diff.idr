module Diff 

import VNode 
import Render
import Attributes

%foreign "browser:lambda: node => node.remove()"
prim__remove : NodeRef -> PrimIO ()

%foreign "browser:lambda: node => node.attributes.forEach(attr => node.removeAttribute(attr))"
prim__removeAttrs : NodeRef -> PrimIO ()

reRender : (root: NodeRef) -> (old : DOM L) -> (new : DOM V) -> IO (DOM L)
reRender root (LiveDOM _ _ oldRef) new = do 
  _ <- primIO $ prim__remove oldRef
  render root new

-- Currently this does nothing 
updateMeta : (new : DOM V) -> (old : DOM L) -> IO (DOM L)
updateMeta new old = pure old

resetAttrs : (old : DOM L) -> List Attribute -> IO (DOM L)
resetAttrs old@(LiveDOM _ _ ref) attrs = do
  _ <- primIO $ prim__removeAttrs ref
  _ <- setAttributes ref attrs
  pure old

-- Checks if we need to update the attributes 
updateAttributes : (new : DOM V) -> (old : DOM L) -> IO (DOM L)
updateAttributes new@(VDOM _ newNode) old@(LiveDOM _ oldNode ref) = 
  case (newNode, oldNode) of 
   (Tag _ oldAttr _, Tag _ newAttr _) => 
     if oldAttr /= newAttr 
        then resetAttrs old newAttr
        else pure old
   (Void _ oldAttr, Void _ newAttr) => 
     if oldAttr /= newAttr
        then resetAttrs old newAttr
        else pure old
   (Text oldText, Text newText) => pure old 
   (_, _) => pure old

mutual 
  diffChildren : (root : NodeRef) -> (old : List (DOM L)) -> (new : List (DOM V)) -> IO (List (DOM L))
  diffChildren root (old :: olds) (new :: news) = do 
    this <-  diff root new old 
    rest <- diffChildren root olds news
    pure (this :: rest)
  diffChildren root ((LiveDOM _ _ ref) :: olds) [] = do
    _ <- primIO $ prim__remove ref
    rest <- diffChildren root olds []
    pure rest
  diffChildren root [] (new :: news) = do 
    this <- render root new
    rest <- diffChildren root [] news
    pure (this :: rest)
  diffChildren root [] [] = pure []

  -- Checks if we need to re-render the whole Node
  reRenderAll : (root : NodeRef) ->  (new : DOM V) -> (old : DOM L) -> IO (DOM L)
  reRenderAll root new@(VDOM _ newNode) old@(LiveDOM oldMeta oldNode oldRef) = 
    case (oldNode, newNode) of 
     ((Tag oldName oldAttr oldChildren), (Tag newName newAttr newChildren)) => 
        if oldName /= newName 
           then reRender root old new 
           else do 
             newChildren <- diffChildren oldRef oldChildren newChildren 
             let newTag = Tag oldName oldAttr newChildren 
             pure (LiveDOM oldMeta newTag oldRef)
     ((Void oldName oldAttr), (Void newName newAttr)) => 
        if oldName /= newName 
           then reRender root old new 
           else pure old
     ((Text oldText), (Text newText)) => 
        if oldText /= newText 
           then reRender root old new
           else pure old
     (_, _) => reRender root old new 

  export
  diff : (root : NodeRef) -> (new : DOM V) -> (old : DOM L) -> IO (DOM L)
  diff root new old = updateMeta new old >>= reRenderAll root new >>= updateAttributes new

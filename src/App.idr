module App

import VNode
import Render 
import Diff

record App msg model where  
  constructor MkApp 
  view : model -> DOM V
  update : model -> msg -> model 
  state : model

-- We'll want to abstract this into an interface, but for now let's just make it work in the 
-- simple case 

create : AnyPtr -> App msg model -> IO (DOM L)
create root app = do
  let vdom = app.view app.state
  render root vdom

doUpdate : AnyPtr -> DOM L -> model -> msg -> App msg model -> IO (DOM L)
doUpdate root old model msg app = do
  let newModel = app.update model msg
  let newVDOM = app.view newModel
  diff 






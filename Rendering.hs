-- Provides class and monad for representing riddle board in console.
module Rendering where

class Renderable a
  where
    render :: a -> String
    
doRender :: Renderable a => a -> IO()
doRender renderable = putStr $ render renderable

module Rendering where

class Renderable a
  where
    render :: a -> String
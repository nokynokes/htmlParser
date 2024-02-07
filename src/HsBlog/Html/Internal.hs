{-# LANGUAGE InstanceSigs #-}
module HsBlog.Html.Internal where

import Numeric.Natural
newtype Html = Html String
newtype Structure = Structure String
type Title = String

instance Semigroup Structure where
  (<>) :: Structure -> Structure -> Structure
  (<>) (Structure c1) (Structure c2) = Structure (c1 <> c2)

instance Monoid Structure where
  mempty :: Structure
  mempty = empty_

empty_ :: Structure
empty_ = Structure ""

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

h2_ :: String -> Structure
h2_ = Structure . el "h2" . escape

h_ :: Natural -> String -> Structure
h_ heading = Structure . el ("h" <> show heading) . escape

li_ :: Structure -> String
li_ = el "li" . getStructureString

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . list_ "ul"

ol_ :: [Structure] -> Structure
ol_ = Structure . list_ "ol"

list_ :: String -> [Structure] -> String
list_ order = el order . concatMap li_

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape = 
  let
    escapeChar c = 
      case c of 
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concatMap escapeChar
    
render :: Html -> String
render (Html str) = str

el :: String -> String -> String
el tag content = 
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
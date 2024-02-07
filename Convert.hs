module Convert where

import qualified Html
import qualified Markup

convertStructure :: Markup.Html -> Html.Structure
convertStructure markup = 
  case markup of 
    Heading nat content -> Html.h_ nat content
    Paragraph content -> Html.p_ content
    UnordedList content -> Html.ul_ $ map Html.p_ content 
    OrderedList content -> Html.ol_ $ map Html.p_ content 
    CodeBlock content -> Html.code_ $ unlines content


concatStructure :: [Html.Structure] -> Html.Structure
concatStructure list = 
  case list of
    [] -> empty_
    x :: xs -> x <> concatStructure xs

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
module Convert where

import qualified Html
import qualified Markup

convertStructure :: Markup.Structure -> Html.Structure
convertStructure markup = 
  case markup of 
    Markup.Heading nat content -> Html.h_ nat content
    Markup.Paragraph content -> Html.p_ content
    Markup.UnordedList content -> Html.ul_ $ map Html.p_ content 
    Markup.OrderedList content -> Html.ol_ $ map Html.p_ content 
    Markup.CodeBlock content -> Html.code_ $ unlines content

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
module Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural (Natural)
import Data.Maybe (maybeToList)
type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnordedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq,Show)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    let preappend = maybe id (:) in
      case txts of
        -- done case
        [] -> maybeToList context

        -- Heading 1 case
        ('*' : ' ' : line) : rest -> 
          preappend context (Heading 1 (trim line) : parseLines Nothing rest)

        -- Unorded list case
        ('-' : ' ' : line) : rest -> 
          case context of
            Just (UnordedList list) ->
              parseLines (Just (UnordedList (list <> [trim line]))) rest
            Nothing -> preappend context (parseLines (Just (UnordedList [trim line])) rest)

        -- Ordered List case
        ('#' : ' ' : line) : rest ->
          case context of
            Just (OrderedList list) ->
              parseLines (Just (UnordedList (list <> [trim line]))) rest
            Nothing -> preappend context (parseLines (Just (UnordedList [trim line])) rest)
        
        -- Code case
        ('>' : ' ' : line) : rest ->
          case context of 
            Just (CodeBlock code) ->
              parseLines (Just (CodeBlock (code <> [line]))) rest
            Nothing -> preappend context (parseLines (Just (CodeBlock [trim line])) rest)
        
        -- Paragraph case
        currentLine : rest ->
          let 
            line = trim currentLine
          in
            if line == ""
              then
                preappend context (parseLines Nothing rest)
              else
                case context of
                  Just (Paragraph p) ->
                    parseLines (Just (Paragraph (unwords [p, line]))) rest
                  Nothing -> preappend context (parseLines (Just (Paragraph line)) rest)
                    

trim :: String -> String
trim = unwords . words
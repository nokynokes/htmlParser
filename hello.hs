import Html

myhtml :: Html
myhtml = 
  html_ 
    "hello, title!" 
    (h1_ "hello & world!" <> p_ "lets learn!")



main :: IO ()
main = putStrLn (render myhtml)
import Parsing.Parse
import Parsing.Interpret
import Types.Expressions
import Types.Stately
import System.Environment
import Text.Parsec

main :: IO ()
main = do
  (file:_) <- getArgs
  f <- readFile file
  case (parse bigParse "propo-interpreter" f) of
    Left  b        -> putStrLn $ show b
    Right Nothing  -> putStrLn "Parsing error (Nothing)"
    Right (Just a) -> putStrLn $ show a

import qualified Parser as P

main :: IO ()
main = do
    let result = P.parseExpr "\\x -> x + 1.0"
    print result


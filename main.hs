import LCExpr
import Parser

runTests :: IO ()
runTests = do
    putStrLn (showExpr (parse "-7"))
    putStrLn (showExpr (parse "x"))
    putStrLn (showExpr (parse "(x 7)"))
    putStrLn (showExpr (parse "(println 7)"))
    putStrLn (showExpr (parse "(ifleq0 1 2 3)"))
    putStrLn (showExpr (parse "(/ x => 7)"))
    putStrLn (showExpr (parse "(println (7 x))"))
    putStrLn (showExpr (parse "((1 2) (3 4))"))
    putStrLn (showExpr (parse "((/ x => (+ x 1)) 8)"))
    putStrLn (showExpr (parse "(/ x => (/ y => (+ x y)))"))

emitLC :: String -> IO ()
emitLC input = do
    putStrLn ("LC: " ++ showExpr (parse input))

emitJS :: String -> IO ()
emitJS input = do
    putStrLn "// To be run in Node.js"
    putStrLn "const readline = require(\"readline\");"
    putStrLn "const r1 = readline.createInterface({"
    putStrLn "    input: process.stdin,"
    putStrLn "    output: process.stdout"
    putStrLn "});"
    putStrLn "r1.question(\"Please enter a number: \", (input) => {"
    putStrLn ("    (" ++ showExprJS (parse input) ++ ")(input);")
    putStrLn "});"

emitPy :: String -> IO ()
emitPy input = do
    putStrLn "def println(str):"
    putStrLn "    print(str)"
    putStrLn "    return str"
    putStrLn "print('Please enter a number: '"
    putStrLn ("(" ++ showExprPy (parse input) ++ ")(input())")

main :: IO ()
main = do
    input <- getLine
    -- emitLC input
    -- emitJS input
    emitPy input
    -- runTests

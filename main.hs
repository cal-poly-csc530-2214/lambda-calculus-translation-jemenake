import LCExpr
-- import Parser

main :: IO ()
main = do
    -- The following AST in mainexp should equate to:
    -- (/ x => println (ifleq0 ((/y => y) 4) 7 x))
   let mainexp = Abs "x" (Println (Ifleq0 (App (Abs "y" (Id "y")) (Num "4")) (Num "7") (Id "x")))
   -- Output the AST in LC-ish syntax
   putStrLn "  ==== The parsed LC sentence is:\n"
   putStrLn (showExpr mainexp)
   -- Output the Javascript program that should ask for user input
   putStrLn "\n  ==== The Javascript version is:\n"
   putStrLn "const readline = require(\"readline\");"
   putStrLn "const r1 = readline.createInterface({"
   putStrLn "    input: process.stdin,"
   putStrLn "    output: process.stdout"
   putStrLn "});"
   putStrLn "while true {"
   putStrLn "    r1.question(\"Please enter a number: \", (input) => {"
   putStrLn ("        " ++ showExprJS mainexp)
   putStrLn "    });"
   putStrLn "}"
   -- Output the Python program that should ask for user input
   putStrLn "\n  ==== The Python version is:\n"
   putStrLn "while true:"
   putStrLn "    print('Please enter a number: '"
   putStrLn ("    (" ++ (showExprPy mainexp) ++ ")(input())")
   putStrLn "done"


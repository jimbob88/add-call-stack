fun1 :: String
fun1 = fun2

fun2 :: String
fun2 = fun3 ++ " " ++ (fun4 "hi")

fun3 :: String
fun3 = "Hello"

fun4 :: () => String -> String
fun4 _ = error "World"

main :: IO ()
main = do
  putStrLn "This is fine!"
  putStrLn fun1
  return ()

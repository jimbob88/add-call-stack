# AddCallStack
This is a simple GHC plugin which adds the `HasCallStack` qualifier to every function to your source code. This is a ground-up rewrite of [haskell-stack-trace-plugin](https://github.com/waddlaw/haskell-stack-trace-plugin) for `GHC2024`, due to differences in the way the GHC AST is built.

## Example
### Example Code
```haskell
-- example/Example.hs
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
```

### Running without the plugin

```
$ cabal run example-without-plugin
This is fine!
example-without-plugin: World
CallStack (from HasCallStack):
  error, called at example/Example.hs:11:10 in add-call-stack-0.1.0.0-inplace-example-without-plugin:Main
HasCallStack backtrace:
  collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:92:13 in ghc-internal:GHC.Internal.Exception
  toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:128:3 in ghc-internal:GHC.Internal.Exception
```

### Running with the plugin
```
$ cabal run example
This is fine!
example: World
CallStack (from HasCallStack):
  error, called at example/Example.hs:11:10 in add-call-stack-0.1.0.0-inplace-example:Main
  fun4, called at example/Example.hs:5:24 in add-call-stack-0.1.0.0-inplace-example:Main
  fun2, called at example/Example.hs:2:8 in add-call-stack-0.1.0.0-inplace-example:Main
  fun1, called at example/Example.hs:16:12 in add-call-stack-0.1.0.0-inplace-example:Main
  main, called at example/Example.hs:14:1 in add-call-stack-0.1.0.0-inplace-example:Main
HasCallStack backtrace:
  collectBacktraces, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:92:13 in ghc-internal:GHC.Internal.Exception
  toExceptionWithBacktrace, called at libraries/ghc-internal/src/GHC/Internal/Exception.hs:128:3 in ghc-internal:GHC.Internal.Exception
```

## Explanation of Change
Recently, there have been two minor changes to the GHC AST which has prevented `haskell-stack-trace-plugin` from functioning.

First of all, GHC removed `HsIB`. This used to contain implicit bindings. It has been superseded by `HsSig` simply directly having  `sig_body` with `LHsType p`. Secondly, various parts of the AST which used to rely on `Located` now use `XRec` wrappers.

Both these changes are dealt with in this implementation.

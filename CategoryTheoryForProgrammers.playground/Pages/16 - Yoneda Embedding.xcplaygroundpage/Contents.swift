/*:
 ```Haskell
 fromY :: (a -> x) -> b -> x
 fromY f b = f (btoa b)
 ```
 ```swift
 func fromY<A, X, B>(_ f: (A) -> X) -> (B) -> X {
    { b in f(bToA(b)) }
 }
 ```
 ................
 ```Haskell
 fromY id :: b -> a
 ```
 ```swift
 fromY(id) : (B) -> A
 ```
 */

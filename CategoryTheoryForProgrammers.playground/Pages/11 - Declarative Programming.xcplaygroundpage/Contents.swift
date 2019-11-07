/*:
 ```Haskell
 h = g . f
 ```
 ```swift
 let h = compose(g, f)
 ```
 ................
 ```Haskell
 h x = let y = f x
       in g y
 ```
 ```swift
 func f<A, B>(_ a: A) -> B
 func g<B, C>(_ b: B) -> C
 
 func h<A, C>(_ x: A) -> C {
    let y = f(x)
    return g(y)
 }
 ```
 */

/*:
 ```Haskell
 f :: A -> B
 ```
 ```swift
 func f<A, B>(_ a: A) -> B
 ```
 ................
 ```Haskell
 g :: B -> C
 ```
 ```swift
 func g<B, C>(_ b: B) -> C
 ```
 ...............
 ```Haskell
 g . f
 ```
 ```swift
 compose(g, f)
 ```
 ..............
 ```Haskell
 f :: A -> B
 g :: B -> C
 h :: C -> D
 h . (g . f) == (h . g) . f == h . g . f
 ```
 ```swift
 func f<A, B>(_ a: A) -> B
 func g<B, C>(_ b: B) -> C
 func h<C, D>(_ c: C) -> D

 compose(h, compose(g, f)) == compose(compose(h, g), f)
 ```
 ..............
 ```Haskell
 id :: a -> a
 id x = x
 ```
 */
func id<A>(_ a: A) -> A {
    a
}
/*:
 ..............
 ```Haskell
 f . id == f
 id . f == f
 ```
 ```swift
 compose(f, id) == f
 compose(id, f) == f
 ```
 ..............
 */

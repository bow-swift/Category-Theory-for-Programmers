/*:
 ```Haskell
 a -> (b -> c)
 ```
 ```swift
 (A) -> (B) -> C
 ```
 ................
 ```Haskell
 a -> b -> c
 ```
 ```swift
 (A) -> (B) -> C
 ```
 ................
 ```Haskell
 catstr :: String -> String -> String
 catstr s s' = s ++ s'
 ```
 */
func catstr(_ s1: String, _ s2: String) -> String {
    s1 + s2
}
 /*:
 ................
 ```Haskell
 catstr' s = \s' -> s ++ s'
 ```
 */
func catstr2(_ s1: String) -> (String) -> String {
    { s2 in s1 + s2 }
}
 /*:
 ................
 ```Haskell
 greet :: String -> String
 greet = catstr "Hello "
 ```
 */
func greet(_ s: String) -> String {
    catstr("Hello ", s)
}
 /*:
 ................
 ```Haskell
 (a, b) -> c
 ```
 ```swift
 (A, B) -> C
 ```
 ................
 ```Haskell
 curry :: ((a, b)->c) -> (a->b->c)
 curry f a b = f (a, b)
 ```
 */
func curry<A, B, C>(_ f: @escaping (A, B) -> C) -> (A) -> (B) -> C {
    { a in { b in f(a, b) } }
}
 /*:
 ................
 ```Haskell
 uncurry :: (a->b->c) -> ((a, b)->c)
 uncurry f (a, b) = f a b
 ```
 */
func uncurry<A, B, C>(_ f: @escaping (A) -> (B) -> C) -> (A, B) -> C {
    { a, b in f(a)(b) }
}
 /*:
 ................
 ```Haskell
 factorizer :: ((a, b)->c) -> (a->(b->c))
 factorizer g = \a -> (\b -> g (a, b))
 ```
 */
func factorizer<A, B, C>(_ g: @escaping (A, B) -> C) -> (A) -> (B) -> C {
    { a in { b in g(a, b) } }
}
 /*:
 ................
 ```Haskell
 f :: Either Int Double -> String
 ```
 ```swift
 func f(_ x: Either<Int, Double>) -> String
 ```
 ................
 ```Haskell
 f (Left n)  = if n < 0 then "Negative int" else "Positive int"
 f (Right x) = if x < 0.0 then "Negative double" else "Positive double"
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
func f(_ x: Either<Int, Double>) -> String {
    x.fold(
        { int in int < 0 ? "Negative int" : "Positive int" },
        { double in double < 0 ? "Negative double" : "Positive double" }
    )
}
 /*:
 ................
 ```Haskell
 eval :: ((a -> b), a) -> b
 ```
 ```swift
 func eval<A, B>(_ f: (A) -> B, _ a: A) -> B
 ```
 ................
 ```Haskell
 eval :: ((a -> b), a) -> b
 eval (f, x) = f x
 ```
 */
func eval<A, B>(_ f: (A) -> B, _ a: A) -> B {
    f(a)
}
 /*:
 ................
 ```Haskell
 Either a b -> a
 ```
 ```swift
 (Either<A, B>) -> A
 ```
 ................
 ```Haskell
 absurd :: Void -> a
 ```
 ```swift
 func absurd<A>(_ x: Never) -> A
 ```
 */

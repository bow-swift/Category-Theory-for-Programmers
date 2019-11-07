/*:
 ```Haskell
 p' = p . m
 q' = q . m
 ```
 ```swift
 let p_prime = compose(p, m)
 let q_prime = compose(q, m)
 ```
 ...................
 ```Haskell
 contramap :: (c' -> c) -> (c -> LimD) -> (c' -> LimD)
 contramap f u = u . f
 ```
 ```swift
 func contramap<C1, C2>(_ f: @escaping (C2) -> C1, _ u: @escaping (C1) -> LimD) -> (C2) -> LimD {
   compose(u, f)
 }
 ```
 ..................
 ```Haskell
 f :: a -> b
 g :: a -> b
 ```
 ```swift
 func f<A, B>(_ a: A) -> B
 func g<A, B>(_ a: A) -> B
 ```
 ..................
 ```Haskell
 p :: c -> a
 q :: c -> b
 ```
 ```swift
 func p<C, A>(_ c: C) -> A
 func q<C, B>(_ c: C) -> B
 ```
 ..................
 ```Haskell
 q = f . p
 q = g . p
 ```
 ```swift
 q == compose(f, p)
 q == compose(g, p)
 ```
 ..................
 ```Haskell
 f . p = g . p
 ```
 ```swift
 compose(f, p) == compose(g, p)
 ```
 ..................
 ```Haskell
 f (x, y) = 2 * y + x
 g (x, y) = y - x
 ```
 */
func f(_ x: Int, _ y: Int) -> Int {
    2 * y + x
}

func g(_ x: Int, _ y: Int) -> Int {
    y - x
}
 /*:
 ..................
 ```Haskell
 p t = (t, (-2) * t)
 ```
 */
func p(_ t: Int) -> (Int, Int) {
    (t, -2 * t)
}
 /*:
 ..................
 ```Haskell
 f . p' = g . p'
 ```
 ```swift
 compose(f, p_prime) == compose(g, p_prime)
 ```
 ..................
 ```Haskell
 p'() = (0, 0)
 ```
 */
func p_prime() -> (Int, Int) {
    (0, 0)
}
 /*:
 ..................
 ```Haskell
 p' = p . h
 ```
 ```swift
 let p_prime = compose(p, h)
 ```
 ..................
 ```Haskell
 h () = 0
 ```
 ```swift
 let h = { 0 }
 ```
 ..................
 ```Haskell
 f :: a -> b
 g :: c -> b
 ```
 ```swift
 func f<A, B>(_ a: A) -> B
 func g<C, B>(_ c: C) -> C
 ```
 ..................
 ```Haskell
 p :: d -> a
 q :: d -> c
 r :: d -> b
 ```
 ```swift
 func p<D, A>(_ d: D) -> A
 func q<D, C>(_ d: D) -> C
 func r<D, B>(_ d: D) -> B
 ```
 ..................
 ```Haskell
 g . q = f . p
 ```
 ```swift
 compose(g, q) == compose(f, p)
 ```
 ..................
 ```Haskell
 f x = 1.23
 ```
 ```swift
 let f = { x in 1.23 }
 ```
 ..................
 ```Haskell
 newtype ToString a = ToString (a -> String)
 instance Contravariant ToString where
     contramap f (ToString g) = ToString (g . f)
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
final class ForToString {}
final class ToString<A>: Kind<ForToString, A> {
    let f: (A) -> String
    
    init(_ f: @escaping (A) -> String) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForToString, A>) -> ToString<A> {
    value as! ToString<A>
}
// nef:end

extension ForToString: Contravariant {
    static func contramap<A, B>(_ fa: Kind<ForToString, A>, _ f: @escaping (B) -> A) -> Kind<ForToString, B> {
        let g = fa^.f
        return ToString(compose(g, f))
    }
}
 /*:
 ..................
 ```Haskell
 ToString (Either b c) ~ (b -> String, c -> String)
 ```
 ```swift
 ToString<Either<B, C>> ~ ((B) -> String, (C) -> String)
 ```
 ..................
 ```Haskell
 r -> (a, b) ~ (r -> a, r -> b)
 ```
 ```swift
 (R) -> (A, B) ~ ((R) -> A, (R) -> B)
 ```
 */

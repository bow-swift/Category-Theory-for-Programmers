/*:
 ```Haskell
 swap       :: (a,b) -> (b,a)
 swap (a,b) = (b,a)
 ```
 */
// nef:begin:hidden
import Bow

public protocol Representable: Functor {
    associatedtype Rep
    
    static func tabulate<X>(_ f: @escaping (Rep) -> X) -> Kind<Self, X>
    static func index<X>(_ fx: Kind<Self, X>) -> (Rep) -> X
}
// nef:end
func swap<A, B>(_ x: (A, B)) -> (B, A) {
    let (a, b) = x
    return (b, a)
}
 /*:
 ................
 ```Haskell
 return :: d -> m d
 ```
 ```swift
 func pure<D, M>(_ d: D) -> Kind<M, D>
 ```
 ................
 ```Haskell
 extract :: w c -> c
 ```
 ```swift
 func extract<C, W>(_ wc: Kind<W, C>) -> C
 ```
 ................
 ```Haskell
 class (Functor f, Representable u) =>
       Adjunction f u | f -> u, u -> f where
   unit         :: a -> u (f a)
   counit       :: f (u a) -> a
 ```
 */
// nef:begin:hidden
class Snippet1 {
// nef:end
open class Adjunction<F: Functor, U: Representable> {
    init() {}
    
    func unit<A>(_ a: A) -> Kind<U, Kind<F, A>> {
        fatalError("Implement unit in subclasses")
    }
    
    func counit<A>(_ a: Kind<F, Kind<U, A>>) -> A {
        fatalError("Implement counit in subclasses")
    }
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 class (Functor f, Representable u) =>
       Adjunction f u | f -> u, u -> f where
   leftAdjunct  :: (f a -> b) -> (a -> u b)
   rightAdjunct :: (a -> u b) -> (f a -> b)
 ```
 */
open class Adjunction<F: Functor, U: Representable> {
    init() {}
    
    func leftAdjunct<A, B>(_ f: @escaping (Kind<F, A>) -> B) -> (A) -> Kind<U, B> {
        fatalError("Implement leftAdjunct in subclasses")
    }
    
    func rightAdjunct<A, B>(_ f: @escaping (A) -> Kind<U, B>) -> (Kind<F, A>) -> B {
        fatalError("Implement rightAdjunct in subclasses")
    }
}
 /*:
 ................
 ```Haskell
   unit           = leftAdjunct id
   counit         = rightAdjunct id
   leftAdjunct f  = fmap f . unit
   rightAdjunct f = counit . fmap f
 ```
 */
extension Adjunction {
    func unit<A>(_ a: A) -> Kind<U, Kind<F, A>> {
        leftAdjunct(id)(a)
    }
    
    func counit<A>(_ a: Kind<F, Kind<U, A>>) -> A {
        rightAdjunct(id)(a)
    }
    
    func leftAdjunct<A, B>(_ a: A, _ f: @escaping (Kind<F, A>) -> B) -> Kind<U, B> {
        unit(a).map(f)
    }
    
    func rightAdjunct<A, B>(_ a: Kind<F, A>, _ f: @escaping (A) -> Kind<U, B>) -> B {
        counit(a.map(f))
    }
}
 /*:
 ................
 ```Haskell
 factorizer :: (c -> a) -> (c -> b) -> (c -> (a, b))
 factorizer p q = \x -> (p x, q x)
 ```
 */
func factorizer<A, B, C>(_ p: @escaping (C) -> A, _ q: @escaping (C) -> B) -> (C) -> (A, B) {
    { x in (p(x), q(x)) }
}
 /*:
 ................
 ```Haskell
 fst . factorizer p q = p
 snd . factorizer p q = q
 ```
 ```swift
 { c in c.0 } <<< factorizer(p, q) == p
 { c in c.1 } <<< factorizer(p, q) == q
 ```
 ................
 ```Haskell
 (,) Int Bool ~ (Int, Bool)
 ```
 ```swift
 Tuple<Int, Bool> ~ (Int, Bool)
 ```
 */

/*:
 ```Haskell
 type Reader a x = a -> x
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
final class ForReader {}
final class ReaderPartial<A>: Kind<ForReader, A> {}
final class Reader<A, X>: Kind<ReaderPartial<A>, X> {
    let f: (A) -> X
    
    init(_ f: @escaping (A) -> X) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<A, X>(_ value: Kind<ReaderPartial<A>, X>) -> Reader<A, X> {
    value as! Reader<A, X>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance Functor (Reader a) where
     fmap f h = f . h
 ```
 */
extension ReaderPartial: Functor {
    static func map<X, B>(_ fa: Kind<ReaderPartial<A>, X>, _ f: @escaping (X) -> B) -> Kind<ReaderPartial<A>, B> {
        let g = fa^.f
        return Reader { x in f(g(x)) }
    }
}
 /*:
 ................
 ```Haskell
 alpha :: forall x . (a -> x) -> F x
 ```
 ```swift
 func alpha<A, X, F>(_ f: (A) -> X) -> Kind<F, X>
 ```
 ................
 ```Haskell
 forall x . (a -> x) -> F x ≅ F a
 ```
 ```swift
 func alpha<A, X, F>(_ f: (A) -> X) -> Kind<F, X> ≅ Kind<F, A>
 ```
 ................
 ```Haskell
 alpha :: forall x . (a -> x) -> F x
 ```
 ```swift
 func alpha<A, X, F>(_ f: (A) -> X) -> Kind<F, X>
 ```
 ................
 ```Haskell
 alpha id :: F a
 ```
 ```swift
 alpha(id): Kind<F, A>
 ```
 ................
 ```Haskell
 fa :: F a
 ```
 ```swift
 let fa: Kind<F, A>
 ```
 ................
 ```Haskell
 alpha h = fmap h fa
 ```
 ```swift
 alpha(h) = F.map(fa, h)
 ```
 ................
 ```Haskell
 forall r . (a -> r) -> r ≅ a
 ```
 ```swift
 ((A) -> R) -> R ≅ A
 ```
 ................
 ```Haskell
 forall x . (x -> a) -> F x ≅ F a
 ```
 ```swift
 func alpha<A, X, F>(_ f: (A) -> X) -> Kind<F, X> ≅ Kind<F, A>
 ```
 */

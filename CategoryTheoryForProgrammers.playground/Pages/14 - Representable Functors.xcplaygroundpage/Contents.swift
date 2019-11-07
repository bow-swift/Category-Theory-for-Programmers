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
 type Op a x = x -> a
 ```
 */
final class ForOp {}
final class OpPartial<R>: Kind<ForOp, R> {}
final class Op<R, A>: Kind<OpPartial<R>, A> {
    fileprivate let f: (A) -> R
    
    init(_ f: @escaping (A) -> R) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<R, A>(_ value: Kind<OpPartial<R>, A>) -> Op<R, A> {
    value as! Op<R, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance Contravariant (Op a) where
     contramap f h = h . f
 ```
 */
 extension OpPartial: Contravariant {
     static func contramap<A, B>(_ fa: Kind<OpPartial<R>, A>, _ f: @escaping (B) -> A) -> Kind<OpPartial<R>, B> {
         let g = fa^.f
         return Op(g <<< f)
     }
 }
 /*:
 ................
 ```Haskell
 instance Profunctor (->) where
   dimap ab cd bc = cd . bc . ab
   lmap = flip (.)
   rmap = (.)
 ```
 */
extension Reader {
    func dimap<B, C>(_ ba: @escaping (B) -> A, _ xc: @escaping (X) -> C) -> Reader<B, C> {
        Reader<B, C>(ba >>> self.f >>> xc)
    }
}
/*:
 ................
 ```Haskell
 alpha :: forall x. (a -> x) -> F x
 ```
 ```swift
 func alpha<A, X, F>(_ reader: Reader<A, X>) -> Kind<F, X>
 ```
 ................
 ```Haskell
 fmap f . alpha = alpha . fmap f
 ```
 ```swift
 compose(ReaderPartial<A>.lift(f), alpha) ==
   compose(alpha, F.lift(f))
 ```
 ................
 ```Haskell
 fmap f (alpha h) = alpha (f . h)
 ```
 ```swift
 F.map(alpha(h), f) == alpha(compose(f, h))
 ```
 ................
 ```Haskell
 beta :: forall x. F x -> (a -> x)
 ```
 ```swift
 func beta<A, F, X>(_ fx: Kind<F, X>) -> Reader<A, X>
 ```
 ................
 ```Haskell
 alpha :: forall x. (Int -> x) -> [x]
 alpha h = map h [12]
 ```
 */
func alpha<X>(_ h: Reader<Int, X>) -> ArrayK<X> {
    [12].k().map(h.f)^
}
 /*
 ................
 ```Haskell
 map f (map h [12]) = map (f . h) [12]
 ```
 ```swift
 ReaderPartial<Int>.map(F.map([12].k()), h), f) ==
   ForArrayK.map([12].k(), compose(f, h))
 ```
 ................
 ```Haskell
 beta :: forall x. [x] -> (Int -> x)
 ```
 ```swift
 func beta<X>(_ array: ArrayK<X>) -> Reader<Int, X>
 ```
 ................
 ```Haskell
 class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index    :: f x -> Rep f -> x
 ```
 */
protocol Representable {
    associatedtype Rep
    
    static func tabulate<X>(_ f: @escaping (Rep) -> X) -> Kind<Self, X>
    static func index<X>(_ fx: Kind<Self, X>) -> (Rep) -> X
}
/*:
 ................
 ```Haskell
 data Stream x = Cons x (Stream x)
 ```
 */
final class ForStream {}
final class Stream<A>: Kind<ForStream, A> {
    let head: () -> A
    let tail: () -> Stream<A>
    
    init(_ head: @escaping () -> A, _ tail: @escaping () -> Stream<A>) {
        self.head = head
        self.tail = tail
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForStream, A>) -> Stream<A> {
    value as! Stream<A>
}
// nef:end
/*:
 ................
 ```Haskell
 instance Representable Stream where
     type Rep Stream = Integer
     tabulate f = Cons (f 0) (tabulate (f . (+1)))
     index (Cons b bs) n = if n == 0 then b else index bs (n - 1)
 ```
 */
extension ForStream: Representable {
    typealias Rep = Int
    
    static func tabulate<X>(_ f: @escaping (Int) -> X) -> Kind<ForStream, X> {
        Stream({ f(0) }, { tabulate(f <<< { x in x + 1 })^ })
    }
    
    static func index<X>(_ fx: Kind<ForStream, X>) -> (Int) -> X {
        { n in (n == 0) ? fx^.head() : index(fx^.tail())(n - 1) }
    }
}

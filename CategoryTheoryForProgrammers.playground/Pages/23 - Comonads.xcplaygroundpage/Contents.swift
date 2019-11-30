/*:
 ```Haskell
 a -> m b
 ```
 ```swift
 (A) -> Kind<M, B>
 ```
 ................
 ```Haskell
 w a -> b
 ```
 ```swift
 Kind<W, A> -> B
 ```
 ................
 ```Haskell
 (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
 ```
 ```swift
 func after<W, A, B, C>(_ f: @escaping (Kind<W, A>) -> B, _ g: @escaping (Kind<W, B>) -> C) -> (Kind<W, A>) -> C
 ```
 ................
 ```Haskell
 extract :: w a -> a
 ```
 ```swift
 func extract<A>(_ wa: Kind<W, A>) -> A
 ```
 ................
 ```Haskell
 class Functor w => Comonad w where
     (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
     extract :: w a -> a
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
/*:
 ```swift
protocol Comonad: Functor {
    static func after<A, B, C>(_ f: @escaping (Kind<Self, A>) -> B,
                               _ g: @escaping (Kind<Self, B>) -> C) -> (Kind<Self, A>) -> C
    static func extract<A>(_ wa: Kind<Self, A>) -> A
}
 ```
 ................
 ```Haskell
 (a, e) -> b
 ```
 ```swift
 (A, E) -> B
 ```
 ................
 ```Haskell
 a -> (e -> b)
 ```
 ```swift
 (A) -> (E) -> B
 ```
 ................
 ```Haskell
 data Product e a = P e a
   deriving Functor
 ```
 */
final class ForProduct {}
final class ProductPartial<E>: Kind<ForProduct, E> {}
final class Product<E, A>: Kind<ProductPartial<E>, A> {
    let value: (E, A)
    
    init(_ value: (E, A)) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<E, A>(_ value: Kind<ProductPartial<E>, A>) -> Product<E, A> {
    value as! Product<E, A>
}
// nef:end
extension ProductPartial: Functor {
    static func map<A, B>(_ fa: Kind<ProductPartial<E>, A>, _ f: @escaping (A) -> B) -> Kind<ProductPartial<E>, B> {
        Product((fa^.value.0, f(fa^.value.1)))
    }
}
 /*:
 ................
 ```Haskell
 (=>=) :: (Product e a -> b) -> (Product e b -> c) -> (Product e a -> c)
 f =>= g = \(P e a) -> let b = f (P e a)
                           c = g (P e b)
                        in c
 ```
 */
func after<E, A, B, C>(_ f: @escaping (Product<E, A>) -> B,
                       _ g: @escaping (Product<E, B>) -> C) -> (Product<E, A>) -> C {
    { pea in
        let b = f(pea)
        let c = g(Product((pea.value.0, b)))
        return c
    }
}
 /*:
 ................
 ```Haskell
 extract (P e a) = a
 ```
 */
func extract<E, A>(_ pea: Product<E, A>) -> A {
    pea.value.1
}
 /*:
 ................
 ```Haskell
 (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
 f =>= g = g ...
 ```
 ```swift
 func after<W, A, B, C>(_ f: @escaping (Kind<W, A>) -> B,
                        _ g: @escaping (Kind<W, B>) -> C) -> (Kind<W, A>) -> C {
     g ...
 }
 ```
 ................
 ```Haskell
 extend :: (w a -> b) -> w a -> w b
 ```
 ```swift
 func coflatMap<W, A, B>(_ wa: Kind<W, A>, _ f: @escaping (Kind<W, A>) -> B) -> Kind<W, B>
 ```
 ................
 ```Haskell
 f =>= g = g . extend f
 ```
 ```swift
 func after<W, A, B, C>(_ f: @escaping (Kind<W, A>) -> B, _ g: @escaping (Kind<W, B>) -> C) -> (Kind<W, A>) -> C {
   g <<< { wa in coflatMap(wa, f) }
 }
 ```
 ................
 ```Haskell
 duplicate :: w a -> w (w a)
 ```
 
 func duplicate<W, A>(_ wa: Kind<W, A>) -> Kind<W, Kind<W, A>>
 ```
 ................
 ```Haskell
 class Functor w => Comonad w where
   extract :: w a -> a
   duplicate :: w a -> w (w a)
   duplicate = extend id
   extend :: (w a -> b) -> w a -> w b
   extend f = fmap f . duplicate
 ```
 */
protocol Comonad: Functor {
    static func extract<A>(_ wa: Kind<Self, A>) -> A
    static func coflatMap<A, B>(_ wa: Kind<Self, A>, _ f: @escaping (Kind<Self, A>) -> B) -> Kind<Self, B>
}

extension Comonad {
    static func duplicate<A>(_ wa: Kind<Self, A>) -> Kind<Self, Kind<Self, A>> {
        coflatMap(wa, id)
    }
}
 /*:
 ................
 ```Haskell
 data Stream a = Cons a (Stream a)
 ```
 */
final class ForStream {}
final class Stream<A>: Kind<ForStream, A> {
    let head: () -> A
    let tail: () -> Stream<A>
    
    init(_ head: @escaping () -> A,
         _ tail: @escaping () -> Stream<A>) {
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
 instance Functor Stream where
     fmap f (Cons a as) = Cons (f a) (fmap f as)
 ```
 */
extension ForStream: Functor {
    static func map<A, B>(_ fa: Kind<ForStream, A>, _ f: @escaping (A) -> B) -> Kind<ForStream, B> {
        Stream({ f(fa^.head()) },
               { fa^.tail().map(f)^ })
    }
}
 /*:
 ................
 ```Haskell
 extract (Cons a _) = a
 ```
 */
func extract<A>(_ wa: Stream<A>) -> A {
    wa.head()
}
 /*:
 ................
 ```Haskell
 duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
 ```
 */
func duplicate<A>(_ wa: Stream<A>) -> Stream<Stream<A>> {
    Stream({ wa },
           { duplicate(wa.tail()) })
}
 /*:
 ................
 ```Haskell
 instance Comonad Stream where
     extract (Cons a _) = a
     duplicate (Cons a as) = Cons (Cons a as) (duplicate as)
 ```
 */
extension ForStream: Comonad {
    static func extract<A>(_ wa: Kind<ForStream, A>) -> A {
        wa^.head()
    }
    
    static func coflatMap<A, B>(_ wa: Kind<ForStream, A>, _ f: @escaping (Kind<ForStream, A>) -> B) -> Kind<ForStream, B> {
        Stream({ f(wa^) },
               { coflatMap(wa^.tail(), f)^ })
    }
}
 /*:
 ................
 ```Haskell
 tail :: Stream a -> Stream a
 tail (Cons a as) = as
 ```
 */
func tail<A>(_ wa: Stream<A>) -> Stream<A> {
    wa.tail()
}
 /*:
 ................
 ```Haskell
 sumS :: Num a => Int -> Stream a -> a
 sumS n (Cons a as) = if n <= 0 then 0 else a + sumS (n - 1) as
 ```
 */
func sumS<A: Numeric>(_ n: Int, _ stream: Stream<A>) -> A {
    (n <= 0) ?
        stream.head() :
        stream.head() + sumS(n - 1, stream.tail())
}
/*:
 ................
 ```Haskell
 average :: Fractional a => Int -> Stream a -> a
 average n stm = (sumS n stm) / (fromIntegral n)
 ```
 */
func average(_ n: Int, _ stream: Stream<Double>) -> Double {
    sumS(n, stream) / Double(n)
}
 /*:
 ................
 ```Haskell
 movingAvg :: Fractional a => Int -> Stream a -> Stream a
 movingAvg n = extend (average n)
 ```
 */
func movingAvg(_ n: Int, _ stream: Stream<Double>) -> Stream<Double> {
    ForStream.coflatMap(stream) { s in average(n, s^) }^
}
 /*:
 ................
 ```Haskell
 class Comonoid m where
   split   :: m -> (m, m)
   destroy :: m -> ()
 ```
 */
protocol Comonoid {
    func split() -> (Self, Self)
    func destroy() -> ()
}
 /*:
 ................
 ```Haskell
 destroy _ = ()
 ```
 */
func destroy<M>(_ x: M) -> () {}
 /*:
 ................
 ```Haskell
 split x = (f x, g x)
 ```
 */
// nef:begin:hidden
func f<M>(_ m: M) -> M { return m }
func g<M>(_ m: M) -> M { return m }
// nef:end
func split<M>(_ x: M) -> (M, M) {
    (f(x), g(x))
}
 /*:
 ................
 ```Haskell
 lambda . bimap destroy id . split = id
 rho . bimap id destroy . split = id
 ```
 ```swift
 compose(lambda, compose(bimap(destroy, id), split)) == id

 compose(rho, compose(bimap(id, destroy), split)) == id
 ```
 ................
 ```Haskell
 lambda (bimap destroy id (split x))
 = lambda (bimap destroy id (f x, g x))
 = lambda ((), g x)
 = g x
 ```
 ```scala
 lambda(bimap(destroy, id))(split(x)) ==
   lambda(bimap(destroy, id))((f(x), g(x))) ==
   lambda(((), g(x))) ==
   g(x)
 ```
 ................
 ```Haskell
 split x = (x, x)
 ```
 ```swift
func split<M>(_ x: M) -> (M, M) {
    return (x, x)
}
 ```
 ................
 ```Haskell
 data Store s a = Store (s -> a) s
 ```
 */
final class ForStore {}
final class StorePartial<S>: Kind<ForStore, S> {}
final class Store<S, A>: Kind<StorePartial<S>, A> {
    let f: (S) -> A
    let s: S
    
    init(_ f: @escaping (S) -> A, _ s: S) {
        self.f = f
        self.s = s
    }
}
// nef:begin:hidden
postfix func ^<S, A>(_ value: Kind<StorePartial<S>, A>) -> Store<S, A> {
    value as! Store<S, A>
}
// nef:begin
 /*:
 ................
 ```Haskell
 counit (Prod (Reader f, s)) = f s
 ```
 */
func counit<S, A>(_ a: Product<S, Reader<S, A>>) -> A {
    let f = a.value.1
    let s = a.value.0
    return f.invoke(s)^.value
}
 /*:
 ................
 ```Haskell
 extract (Store f s) = f s
 ```
 */
func extract<S, A>(_ wa: Store<S, A>) -> A {
    wa.f(wa.s)
}
 /*:
 ................
 ```Haskell
 unit a = Reader (\s -> Prod (a, s))
 ```
 */
// nef:begin:hidden
extension Kleisli where F == ForId {
    convenience init(_ f: @escaping (D) -> A) {
        self.init { d in Id(f(d)) }
    }
}
// nef:end
func unit<S, A>(_ a: A) -> Reader<S, Product<S, A>> {
    Reader { s in Product((s, a)) }
}
 /*:
 ................
 ```Haskell
 Store f :: s -> Store f s
 ```
 */
extension Store {
    static func apply(_ f: @escaping (S) -> A) -> (S) -> Store<S, A> {
        { s in Store(f, s) }
    }
}
 /*:
 ................
 ```Haskell
 duplicate (Store f s) = Store (Store f) s
 ```
 */
func duplicate<S, A>(_ wa: Store<S, A>) -> Store<S, Store<S, A>> {
    Store({ s in Store(wa.f, s) }, wa.s)
}
/*:
 ................
 ```Haskell
 instance Comonad (Store s) where
   extract (Store f s) = f s
   duplicate (Store f s) = Store (Store f) s
 ```
 */
// nef:begin:hidden
extension StorePartial: Functor {
    static func map<A, B>(_ fa: Kind<StorePartial<S>, A>, _ f: @escaping (A) -> B) -> Kind<StorePartial<S>, B> {
        Store({ s in f(fa^.f(fa^.s)) }, fa^.s)
    }
}
// nef:end
extension StorePartial: Comonad {
    static func extract<A>(_ fa: Kind<StorePartial<S>, A>) -> A {
        fa^.f(fa^.s)
    }
    
    static func coflatMap<A, B>(_ fa: Kind<StorePartial<S>, A>, _ f: @escaping (Kind<StorePartial<S>, A>) -> B) -> Kind<StorePartial<S>, B> {
        Store({ s in Store(fa^.f, s) }, fa^.s).map(f)
    }
}
/*:
 ................
 ```Haskell
 a -> Store s a
 ```
 ```swift
 (A) -> Store<S, A>
 ```
 ................
 ```Haskell
 set :: a -> s -> a
 get :: a -> s
 ```
 ```swift
 func set<A, S>(_ a: A, _ s: S) -> A
 func get<A, S>(_ a: A) -> S
 ```
 */

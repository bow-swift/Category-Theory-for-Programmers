/*:
 ```Haskell
 class Bifunctor f where
     bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
     bimap g h = first g . second h
     first :: (a -> c) -> f a b -> f c b
     first g = bimap g id
     second :: (b -> d) -> f a b -> f a d
     second = bimap id
 ```
 */
protocol Bifunctor {
    static func bimap<A, B, C, D>(_ fab: Kind2<Self, A, B>, _ f: @escaping (A) -> C, _ g: @escaping (B) -> D) -> Kind2<Self, C, D>
}

extension Bifunctor {
    static func first<A, B, C>(_ fab: Kind2<Self, A, B>, _ f: @escaping (A) -> C) -> Kind2<Self, C, B> {
        bimap(fab, f, id)
    }
    
    static func second<A, B, D>(_ fab: Kind2<Self, A, B>, _ g: @escaping (B) -> D) -> Kind2<Self, A, D> {
        bimap(fab, id, g)
    }
}
 /*:
 ................
 ```Haskell
 instance Bifunctor (,) where
     bimap f g (x, y) = (f x, g y)
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
final class ForTuple {}
final class Tuple<A, B>: Kind2<ForTuple, A, B> {
    let first: A
    let second: B
    
    init(_ first: A, _ second: B) {
        self.first = first
        self.second = second
    }
}
// nef:begin:hidden
postfix func ^<A, B>(_ value: Kind2<ForTuple, A, B>) -> Tuple<A, B> {
    value as! Tuple<A, B>
}
// nef:end
extension ForTuple: Bifunctor {
    static func bimap<A, B, C, D>(_ fab: Kind2<ForTuple, A, B>, _ f: @escaping (A) -> C, _ g: @escaping (B) -> D) -> Kind2<ForTuple, C, D> {
        Tuple(f(fab^.first), g(fab^.second))
    }
}
 /*:
 ................
 ```Haskell
 bimap :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
 ```
 ```swift
 func bimap<A, B, C, D>(_ f: @escaping (A) -> C, _ g: @escaping (B) -> C) -> (Kind2<ForTuple, A, B>) -> Kind2<ForTuple, C, D>
 ```
 ................
 ```Haskell
 instance Bifunctor Either where
     bimap f _ (Left x)  = Left (f x)
     bimap _ g (Right y) = Right (g y)
 ```
 */
func bimap<A, B, C, D>(_ fab: Either<A, B>, _ f: @escaping (A) -> C, _ g: @escaping (B) -> D) -> Either<C, D> {
    fab.fold({ a in .left(f(a)) },
             { b in .right(g(b)) })
}
 /*:
 ................
 ```Haskell
 data Identity a = Identity a
 ```
 */
final class ForId {}
final class Id<A>: Kind<ForId, A> {
    let value: A
    
    init(_ value: A) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForId, A>) -> Id<A> {
    value as! Id<A>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance Functor Identity where
     fmap f (Identity x) = Identity (f x)
 ```
 */
extension ForId: Functor {
    static func map<A, B>(_ fa: Kind<ForId, A>, _ f: @escaping (A) -> B) -> Kind<ForId, B> {
        Id(f(fa^.value))
    }
}
 /*:
 ................
 ```Haskell
 data Maybe a = Nothing | Just a
 ```
 */
final class ForOption {}
final class Option<A>: Kind<ForOption, A> {
    fileprivate let value: A?
    
    static func some(_ a: A) -> Option<A> {
        Option(value: a)
    }
    
    static func none() -> Option<A> {
        Option(value: nil)
    }
    
    private init(value: A?) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForOption, A>) -> Option<A> {
    value as! Option<A>
}
// nef:end
 /*:
 ................
 ```Haskell
 type Maybe a = Either (Const () a) (Identity a)
 ```
 ```swift
 typealias Option<A> = Either<Const<(), A>, Id<A>>
 ```
 ................
 ```Haskell
 newtype BiComp bf fu gu a b = BiComp (bf (fu a) (gu b))
 ```
 */
final class ForBiComp {}
final class BiCompPartial<BF, FU, GU>: Kind3<ForBiComp, BF, FU, GU> {}
final class BiComp<BF, FU, GU, A, B>: Kind2<BiCompPartial<BF, FU, GU>, A, B> {
    let value: Kind2<BF, Kind<FU, A>, Kind<GU, B>>
    
    init(_ value: Kind2<BF, Kind<FU, A>, Kind<GU, B>>) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<BF, FU, GU, A, B>(_ value: Kind2<BiCompPartial<BF, FU, GU>, A, B>) -> BiComp<BF, FU, GU, A, B> {
    value as! BiComp<BF, FU, GU, A, B>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance (Bifunctor bf, Functor fu, Functor gu) =>
   Bifunctor (BiComp bf fu gu) where
     bimap f1 f2 (BiComp x) = BiComp ((bimap (fmap f1) (fmap f2)) x)
 ```
 */
extension BiCompPartial: Bifunctor where BF: Bifunctor, FU: Functor, GU: Functor {
    static func bimap<A, B, C, D>(_ fab: Kind2<BiCompPartial<BF, FU, GU>, A, B>, _ f: @escaping (A) -> C, _ g: @escaping (B) -> D) -> Kind2<BiCompPartial<BF, FU, GU>, C, D> {
        BiComp(BF.bimap(fab^.value, FU.lift(f), GU.lift(g)))
    }
}
 /*:
 ................
 ```Haskell
 bf (fu a) (gu b)
 ```
 ```swift
 Kind2<BF, Kind<FU, A>, Kind<GU, B>>
 ```
 ................
 ```Haskell
 f1 :: a -> a'
 f2 :: b -> b'
 ```
 ```swift
 func f1<A, A2>(_ a: A) -> A2
 func f2<B, B2>(_ b: B) -> B2
 ```
 ................
 ```Haskell
 bimap :: (fu a -> fu a') -> (gu b -> gu b')
   -> bf (fu a) (gu b) -> bf (fu a') (gu b')
 ```
 ```swift
 func bimap<FU, GU, BF, A, A2, B, B2>(_ f: @escaping (Kind<FU, A>) -> Kind<FU, A2>, _ g: @escaping (Kind<GU, B>) -> Kind<GU, B2>) -> (Kind2<BF, Kind<FU, A>, Kind<GU, B>>) -> Kind2<BF, Kind<FU, A>, Kind<GU, B2>>
 ```
 ................
 ```Haskell
 data Tree a = Leaf a | Node (Tree a) (Tree a)
     deriving Functor
 ```
 */
final class ForTree {}
final class Tree<A>: Kind<ForTree, A> {
    enum _Tree<A> {
        case leaf(A)
        case node(A, Tree<A>, Tree<A>)
    }
    
    static func leaf(_ a: A) -> Tree<A> {
        Tree(.leaf(a))
    }
    
    static func node(_ a: A, _ left: Tree<A>, _ right: Tree<A>) -> Tree<A> {
        Tree(.node(a, left, right))
    }
    
    fileprivate let value: _Tree<A>
    
    private init(_ value: _Tree<A>) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForTree, A>) -> Tree<A> {
    value as! Tree<A>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance Functor Tree where
     fmap f (Leaf a) = Leaf (f a)
     fmap f (Node t t') = Node (fmap f t) (fmap f t')
 ```
 */
 extension ForTree: Functor {
     static func map<A, B>(_ fa: Kind<ForTree, A>, _ f: @escaping (A) -> B) -> Kind<ForTree, B> {
         switch fa^.value {
         case let .leaf(a):
            return Tree.leaf(f(a))
         case let .node(a, left, right):
             return Tree.node(f(a), left.map(f)^, right.map(f)^)
         }
     }
 }
/*:
 ................
 ```Haskell
 type Writer a = (a, String)
 ```
 */
 typealias Writer<A> = (A, String)
 /*:
 ................
 ```Haskell
 (>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
 m1 >=> m2 = \x ->
     let (y, s1) = m1 x
         (z, s2) = m2 y
     in (z, s1 ++ s2)
 ```
 */
func andThen<A, B, C>(_ m1: @escaping (A) -> Writer<B>, _ m2: @escaping (B) -> Writer<C>) -> (A) -> Writer<C> {
    { x in
        let (y, s1) = m1(x)
        let (z, s2) = m2(y)
        return (z, s1 + s2)
    }
}
 /*:
 ................
 ```Haskell
 return :: a -> Writer a
 return x = (x, "")
 ```
 */
func pure<A>(_ x: A) -> Writer<A> {
    (x, "")
}
 /*:
 ................
 ```Haskell
 fmap f = id >=> (\x -> return (f x))
 ```
 */
func fmap<A, B>(_ f: @escaping (A) -> B) -> (Writer<A>) -> (Writer<B>) {
    { wa in
        andThen(id, { x in pure(f(x)) })(wa)
    }
}
 /*:
 ................
 ```Haskell
 (->) r
 ```
 */
final class ForReader {}
final class ReaderPartial<R>: Kind<ForReader, R> {}
 /*:
 ................
 ```Haskell
 type Reader r a = r -> a
 ```
 */
final class Reader<R, A>: Kind<ReaderPartial<R>, A> {
    fileprivate let f: (R) -> A
    
    init(_ f: @escaping (R) -> A) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<R, A>(_ value: Kind<ReaderPartial<R>, A>) -> Reader<R, A> {
    value as! Reader<R, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance Functor (Reader r) where
     fmap f g = f . g
 ```
 */
extension ReaderPartial: Functor {
    static func map<A, B>(_ fa: Kind<ReaderPartial<R>, A>, _ f: @escaping (A) -> B) -> Kind<ReaderPartial<R>, B> {
        Reader(fa^.f >>> f)
    }
}
 /*:
 ................
 ```Haskell
 type Op r a = a -> r
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
 fmap :: (a -> b) -> (a -> r) -> (b -> r)
 ```
 ```swift
 func map<A, B>(_ fa: Op<R, A>, _ f: @escaping (A) -> B) -> Op<R, B>
 // Equivalent to map<A, B>(_ fa : (A) -> R, _ f : (A) -> B) -> (B) -> R
 ```
 ................
 ```Haskell
 class Contravariant f where
     contramap :: (b -> a) -> (f a -> f b)
 ```
 */
protocol Contravariant {
    static func contramap<A, B>(_ fa: Kind<Self, A>, _ f: @escaping (B) -> A) -> Kind<Self, B>
}
 /*:
 ................
 ```Haskell
 instance Contravariant (Op r) where
     -- (b -> a) -> Op r a -> Op r b
     contramap f g = g . f
 ```
 */
extension OpPartial: Contravariant {
    static func contramap<A, B>(_ fa: Kind<OpPartial<R>, A>, _ f: @escaping (B) -> A) -> Kind<OpPartial<R>, B> {
        Op(fa^.f <<< f)
    }
}
 /*:
 ................
 ```Haskell
 flip :: (a -> b -> c) -> (b -> a -> c)
 flip f y x = f x y
 ```
 */
func flip<A, B, C>(_ f: @escaping (A, B) -> C) -> (B, A) -> C {
    { b, a in f(a, b) }
}
 /*:
 ................
 ```Haskell
 contramap = flip (.)
 ```
 */
func contramap<R, A, B>(_ fa: Op<R, A>, _ f: @escaping (B) -> A) -> Op<R, B> {
    Op(flip(compose)(f, fa.f))
}
 /*:
 ................
 ```Haskell
 class Profunctor p where
   dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
   dimap f g = lmap f . rmap g
   lmap :: (a -> b) -> p b c -> p a c
   lmap f = dimap f id
   rmap :: (b -> c) -> p a b -> p a c
   rmap = dimap id
 ```
 */
protocol Profunctor {
    static func dimap<A, B, C, D>(_ pbc: Kind2<Self, B, C>, _ f: @escaping (A) -> B, _ g: @escaping (C) -> D) -> Kind2<Self, A, D>
}

extension Profunctor {
    static func lmap<A, B, C>(_ pbc: Kind2<Self, B, C>, _ f: @escaping (A) -> B) -> Kind2<Self, A, C> {
        dimap(pbc, f, id)
    }
    
    static func rmap<A, B, C>(_ pab: Kind2<Self, A, B>, _ f: @escaping (B) -> C) -> Kind2<Self, A, C> {
        dimap(pab, id, f)
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
    func dimap<C, D>(_ f: @escaping (C) -> R, _ g: @escaping (A) -> D) -> Reader<C, D> {
        Reader<C, D>(f >>> self.f >>> g)
    }
}

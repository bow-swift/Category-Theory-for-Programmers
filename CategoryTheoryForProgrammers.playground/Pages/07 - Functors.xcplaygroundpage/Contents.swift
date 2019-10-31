/*:
 ................
 ```Haskell
 data Maybe a = Nothing | Just a
 ```
 */
// nef:begin:hidden
class SnippetNoHK {
// nef:end
enum Option<A> {
    case none
    case some(A)
}
 /*:
 ................
 ```Haskell
 f :: a -> b
 ```
 ```swift
 func f<A, B>(_ a: A) -> B
 ```
 ................
 ```Haskell
 f’ :: Maybe a -> Maybe b
 f’ Nothing = Nothing
 f’ (Just x) = Just (f x)
 ```
 */
// nef:begin:hidden
func f<A, B>(_ a: A) -> B { fatalError() }
// nef:end
func f_prime<A, B>(_ option: Option<A>) -> Option<B> {
    switch(option) {
    case .none: return .none
    case let .some(x): return .some(f(x))
    }
}
 /*:
 ................
 ```Haskell
 fmap :: (a -> b) -> (Maybe a -> Maybe b)
 ```
 ```swift
 func lift<A, B>(_ f: @escaping (A) -> B) -> (Option<A>) -> Option<B>
 ```
 ................
 ```Haskell
 fmap :: (a -> b) -> Maybe a -> Maybe b
 ```
 ```swift
 func lift<A, B>(_ f: @escaping (A) -> B, _ fa: Option<A>) -> Option<B>
 ```
 ................
 ```Haskell
 fmap _ Nothing = Nothing
 fmap f (Just x) = Just (f x)
 ```
 */
func lift<A, B>(_ f: @escaping (A) -> B, _ fa: Option<A>) -> Option<B> {
    switch(fa) {
    case .none: return .none
    case let .some(x): return .some(f(x))
    }
}
 /*:
 ................
 ```Haskell
 id x = x
 ```
 */
func id<A>(_ x: A) -> A {
    return x
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 fmap id = id
 ```
 ```swift
 lift(id) == id
 ```
 ................
 ```Haskell
 fmap (g . f) = fmap g . fmap f
 ```
 ```swift
 lift(compose(g, f)) == compose(lift(g), lift(f))
 ```
 ................
 ```Haskell
 class Eq a where
     (==) :: a -> a -> Bool
 ```
 */
protocol Eq {
    static func eqv(_ x: Self, _ y: Self) -> Bool
}
 /*:
 ................
 ```Haskell
 data Point = Pt Float Float
 ```
 */
struct Point {
    let x: Float
    let y: Float
}
 /*:
 ................
 ```Haskell
 instance Eq Point where
     (Pt x y) == (Pt x' y') = x == x' && y == y'
 ```
 */
extension Point: Eq {
    static func eqv(_ a1: Point, _ a2: Point) -> Bool {
        a1.x == a2.x && a1.y == a2.y
    }
}
 /*:
 ................
 ```Haskell
 class Functor f where
     fmap :: (a -> b) -> f a -> f b
 ```
 */
protocol Functor {
    static func map<A, B>(_ fa: Kind<Self, A>, _ f: @escaping (A) -> B) -> Kind<Self, B>
}

extension Functor {
    static func lift<A, B>(_ f: @escaping (A) -> B) -> (Kind<Self, A>) -> Kind<Self, B> {
        { fa in map(fa, f) }
    }
}

extension Kind where F: Functor {
    func map<B>(_ f: @escaping (A) -> B) -> Kind<F, B> {
        F.map(self, f)
    }
}
 /*:
 ................
 ```Haskell
 instance Functor Maybe where
     fmap _ Nothing = Nothing
     fmap f (Just x) = Just (f x)
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
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

extension ForOption: Functor {
    static func map<A, B>(_ fa: Kind<ForOption, A>, _ f: @escaping (A) -> B) -> Kind<ForOption, B> {
        switch(fa^.value) {
        case let .some(a): return Option<B>.some(f(a))
        case .none: return Option<B>.none()
        }
    }
}
 /*:
 ................
 ```Haskell
 data List a = Nil | Cons a (List a)
 ```
 */
final class ForList {}
final class List<A>: Kind<ForList, A> {
    fileprivate enum _List<A> {
        case `nil`
        case cons(A, List<A>)
    }
    
    static func `nil`() -> List<A> {
        List(value: .nil)
    }
    
    static func cons(_ head: A, _ tail: List<A>) -> List<A> {
        List(value: .cons(head, tail))
    }
    
    fileprivate let value: _List<A>
    
    private init(value: _List<A>) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForList, A>) -> List<A> {
    value as! List<A>
}
// nef:end
 /*:
 ................
 ```Haskell
 fmap :: (a -> b) -> (List a -> List b)
 ```
 ```swift
 func lift<A, B>(_ f: @escaping (A) -> B) -> (List<A>) -> List<B>
 ```
 ................
 ```Haskell
 fmap f (Cons x t) = Cons (f x) (fmap f t)
 ```
 */
func lift<A, B>(_ f: @escaping (A) -> B) -> (List<A>) -> List<B> {
    return { fa in
        switch(fa.value) {
        case let .cons(head, tail):
            return .cons(f(head), lift(f)(tail))
        case .nil: return .nil()
        }
    }
}
 /*:
 ................
 ```Haskell
 instance Functor List where
     fmap _ Nil = Nil
     fmap f (Cons x t) = Cons (f x) (fmap f t)
 ```
 */
extension ForList: Functor {
    static func map<A, B>(_ fa: Kind<ForList, A>, _ f: @escaping (A) -> B) -> Kind<ForList, B> {
        switch(fa^.value) {
        case let .cons(head, tail): return List<B>.cons(f(head), map(tail, f)^)
        case .nil: return List<B>.nil()
        }
    }
}
 /*
 ................
 ```Haskell
 (->) a b
 ```
 */
final class ForFunction1 {}
final class Function1Partial<I>: Kind<ForFunction1, I> {}
final class Function1<I, O> : Kind<Function1Partial<I>, O> {
    fileprivate let f: (I) -> O
    
    init(_ f: @escaping (I) -> O) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<I, O>(_ value: Kind<Function1Partial<I>, O>) -> Function1<I, O> {
    value as! Function1<I, O>
}
// nef:end
 /*:
 ................
 ```Haskell
 (->) a
 ```
 ```swift
 final class Function1Partial<I>: Kind<ForFunction1, I> {}
 ```
 ................
 ```Haskell
 fmap :: (a -> b) -> (r -> a) -> (r -> b)
 ```
 ```swift
 func lift<A, B, R>(_ f: @escaping (A) -> B, _ g: Function1<R, A>) -> Function1<R, B>
 ```
 ................
 ```Haskell
 instance Functor ((->) r) where
     fmap f g = f . g
 ```
 */
extension Function1Partial: Functor {
    static func map<A, B>(_ fa: Kind<Function1Partial<I>, A>, _ f: @escaping (A) -> B) -> Kind<Function1Partial<I>, B> {
        Function1<I, B>(compose(f, fa^.f))
    }
}
 /*:
 ................
 ```Haskell
 fmap f g = (.) f g
 ```
 ```swift
 extension Function1Partial: Functor {
     static func map<A, B>(_ fa: Kind<Function1Partial<I>, A>, _ f: @escaping (A) -> B) -> Kind<Function1Partial<I>, B> {
         Function1<I, B>(compose(f, fa^.f))
     }
 }
 ```
 ................
 ```Haskell
 fmap = (.)
 ```
 ```swift
 extension Function1Partial: Functor {
     static func map<A, B>(_ fa: Kind<Function1Partial<I>, A>, _ f: @escaping (A) -> B) -> Kind<Function1Partial<I>, B> {
         Function1<I, B>(compose(f, fa^.f))
     }
 }
 ```
 ................
 ```Haskell
 nats :: [Integer]
 nats = [1..]
 ```
 */
 let nats = (1...).lazy
 /*
 ................
 ```Haskell
 data Const c a = Const c
 ```
 */
final class ForConst {}
final class ConstPartial<C>: Kind<ForConst, C> {}
final class Const<C, A>: Kind<ConstPartial<C>, A> {
    let c: C
    
    init(_ c: C) {
        self.c = c
    }
}
// nef:begin:hidden
postfix func ^<C, A>(_ value: Kind<ConstPartial<C>, A>) -> Const<C, A> {
    value as! Const<C, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 fmap :: (a -> b) -> Const c a -> Const c b
 ```
 ```swift
 func lift<A, B, C>(_ f: @escaping (A) -> B) -> (Const<C, A>) -> Const<C, B>
 ```
 ................
 ```Haskell
 instance Functor (Const c) where
     fmap _ (Const v) = Const v
 ```
 */
extension ConstPartial: Functor {
    static func map<A, B>(_ fa: Kind<ConstPartial<C>, A>, _ f: @escaping (A) -> B) -> Kind<ConstPartial<C>, B> {
        Const<C, B>(fa^.c)
    }
}
 /*:
 ................
 ```Haskell
 maybeTail :: [a] -> Maybe [a]
 maybeTail [] = Nothing
 maybeTail (x:xs) = Just xs
 ```
 */
func maybeTail<A>(_ list: List<A>) -> Option<List<A>> {
    switch(list.value) {
    case let .cons(_, xs): return .some(xs)
    case .nil: return .none()
    }
}
 /*:
 ................
 ```Haskell
 square x = x * x

 mis :: Maybe [Int]
 mis = Just [1, 2, 3]

 mis2 = fmap (fmap square) mis
 ```
 */
func square(_ x: Int) -> Int { x * x }

let mis: Option<List<Int>> = .some(.cons(1, .cons(2, .cons(3, .nil()))))
var mis2 = mis.map { list in list.map(square)^ }^

 /*:
 ................
 ```Haskell
 mis2 = (fmap . fmap) square mis
 ```
 */
func mapComposed<A, B>(_ f: @escaping (A) -> B) -> (Option<List<A>>) -> Option<List<B>> {
    { ola in
        ola.map { la in la.map(f)^ }^
    }
}

 mis2 = mapComposed(square)(mis)
 /*:
 ................
 ```Haskell
 fmap :: (a -> b) -> (f a -> f b)
 ```
 ```swift
 func lift<A, B>(_ f: @escaping (A) -> B) -> (Kind<F, A>) -> Kind<F, B>
 ```
 ................
 ```Haskell
 square :: Int -> Int
 ```
 ```swift
 func square(_ x: Int) -> Int
 ```
 ................
 ```Haskell
 [Int] -> [Int]
 ```
 ```swift
 (List<Int>) -> List<Int>
 ```
 ................
 ```Haskell
 Maybe [Int] -> Maybe [Int]
 ```
 ```swift
 (Option<List<Int>>) -> Option<List<Int>>
 ```
 */

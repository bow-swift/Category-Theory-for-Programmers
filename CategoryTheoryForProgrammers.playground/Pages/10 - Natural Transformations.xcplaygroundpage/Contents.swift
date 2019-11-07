/*:
 ```Haskell
 alphaₐ :: F a -> G a
 ```
 ```swift
 func alphaA<F, G, A>(_ fa: Kind<F, A>) -> Kind<G, A>
 ```
 ................
 ```Haskell
 alpha :: forall a . F a -> G a
 ```
 ```swift
 func alphaA<F, G, A>(_ fa: Kind<F, A>) -> Kind<G, A>
 ```
 ................
 ```Haskell
 alpha :: F a -> G a
 ```
 ```swift
 func alpha<F, G, A>(_ fa: Kind<F, A>) -> Kind<G, A>
 ```
 ................
 ```Haskell
 alpha :: F a -> G a
 ```
 ```swift
 func alpha<F, G, A>(_ fa: Kind<F, A>) -> Kind<G, A>
 ```
 ................
 ```Haskell
 safeHead :: [a] -> Maybe a
 safeHead [] = Nothing
 safeHead (x:xs) = Just x
 ```
 */
// nef:begin:hidden
import Bow

final class ForList {}
typealias ListOf<A> = Kind<ForList, A>
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

postfix func ^<A>(_ value: Kind<ForList, A>) -> List<A> {
    value as! List<A>
}

extension ForList: Functor {
    static func map<A, B>(_ fa: Kind<ForList, A>, _ f: @escaping (A) -> B) -> Kind<ForList, B> {
        switch(fa^.value) {
        case let .cons(head, tail): return List<B>.cons(f(head), map(tail, f)^)
        case .nil: return List<B>.nil()
        }
    }
}
// nef:end
func safeHead<A>(_ list: ListOf<A>) -> OptionOf<A> {
    switch(list^.value) {
    case let .cons(head, _): return Option.some(head)
    case .nil: return Option.none()
    }
}
/*:
 ................
 ```Haskell
 fmap f . safeHead = safeHead . fmap f
 ```
 ```swift
 compose(ForOption.lift(f), safeHead) ==
   compose(safeHead, ForList.lift(f))
 ```
 ................
 ```Haskell
 fmap f (safeHead []) = fmap f Nothing = Nothing
 ```
 ```swift
 ForOption.lift(f)(safeHead(List<A>.nil())) ==
   ForOption.lift(f)(Option<A>.none()) ==
     Option<A>.none()
 ```
 ................
 ```Haskell
 safeHead (fmap f []) = safeHead [] = Nothing
 ```
 ```swift
 safeHead(ForList.lift(f)(List<A>.nil())) ==
   safeHead(List<A>.empty()) ==
     Option<A>.none()
 ```
 ................
 ```Haskell
 fmap f (safeHead (x:xs)) = fmap f (Just x) = Just (f x)
 ```
 ```swift
 ForOption.lift(f)(safeHead(List<A>.cons(x, xs))) ==
   ForOption.lift(f)(Option<A>.some(x)) ==
     Option<A>.some(f(x))
 ```
 ................
 ```Haskell
 safeHead (fmap f (x:xs)) = safeHead (f x : fmap f xs) = Just (f x)
 ```
 ```swift
 safeHead(ForList.lift(f)(List<A>.cons(x, xs))) ==
   safeHead(List<A>.cons(f(x), ForList.lift(f)(xs))) ==
     Option<A>.some(f(x))
 ```
 ................
 ```Haskell
 fmap f [] = []
 fmap f (x:xs) = f x : fmap f xs
 ```
 */
func lift<A, B>(_ f: @escaping (A) -> B) -> (List<A>) -> List<B> {
    { list in ForList.map(list, f)^ }
}
/*:
 ................
 ```Haskell
 fmap f Nothing = Nothing
 fmap f (Just x) = Just (f x)
 ```
 */
func lift<A, B>(_ f: @escaping (A) -> B) -> (Option<A>) -> Option<B> {
    { option in ForOption.map(option, f)^ }
}
/*:
 ................
 ```Haskell
 length :: [a] -> Const Int a
 length [] = Const 0
 length (x:xs) = Const (1 + unConst (length xs))
 ```
 */
func length<A>(_ list: List<A>) -> Const<Int, A> {
    switch(list.value) {
    case let .cons(_, tail):
        return Const(1 + length(tail).value)
    case .nil:
        return Const(0)
    }
}
/*:
 ................
 ```Haskell
 unConst :: Const c a -> c
 unConst (Const x) = x
 ```
 */
func unConst<C, A>(_ x: Const<C, A>) -> C {
    x.value
}
/*:
 ................
 ```Haskell
 length :: [a] -> Int
 ```
 ```swift
 func length<A>(_ list: List<A>) -> Int
 ```
 ................
 ```Haskell
 scam :: Const Int a -> Maybe a
 scam (Const x) = Nothing
 ```
 */
func scam<A>(_ x: Const<Int, A>) -> Option<A> {
    .none()
}
/*:
 ................
 ```Haskell
 newtype Reader e a = Reader (e -> a)
 ```
 */
final class ForReader {}
final class ReaderPartial<E>: Kind<ForReader, E> {}
final class Reader<E, A>: Kind<ReaderPartial<E>, A> {
    fileprivate let f: (E) -> A
    
    init(_ f: @escaping (E) -> A) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<E, A>(_ value: Kind<ReaderPartial<E>, A>) -> Reader<E, A> {
    value as! Reader<E, A>
}
// nef:end
/*:
 ................
 ```Haskell
 instance Functor (Reader e) where
     fmap f (Reader g) = Reader (\x -> f (g x))
 ```
 */
extension ReaderPartial: Functor {
    static func map<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> B) -> Kind<ReaderPartial<E>, B> {
        let g = fa^.f
        return Reader { x in f(g(x)) }
    }
}
/*:
 ................
 ```Haskell
 alpha :: Reader () a -> Maybe a
 ```
 ```swift
 func alpha<A>(_ reader: Reader<(), A>) -> Option<A>
 ```
 ................
 ```Haskell
 dumb (Reader _) = Nothing
 ```
 */
func dumb<A>(_ reader: Reader<(), A>) -> Option<A> {
    .none()
}
/*:
 ................
 ```Haskell
 obvious (Reader g) = Just (g ())
 ```
 */
func obvious<A>(_ reader: Reader<(), A>) -> Option<A> {
    .some(reader.f(()))
}
/*:
 ................
 ```Haskell
 newtype Op r a = Op (a -> r)
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
 instance Contravariant (Op r) where
     contramap f (Op g) = Op (g . f)
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
 predToStr (Op f) = Op (\x -> if f x then "T" else "F")
 ```
 */
func predToStr<A>(_ op: Op<Bool, A>) -> Op<String, A> {
    Op { x in op.f(x) ? "T" : "F" }
}
/*:
 ................
 ```Haskell
 contramap f . predToStr = predToStr . contramap f
 ```
 ```swift
 compose(OpPartial<String>.contralift(f), predToStr) ==
   compose(predToStr, OpPartial<Bool>.contralift(f))
 ```
 ................
 ```Haskell
 contramap :: (b -> a) -> (Op Bool a -> Op Bool b)
 ```
 ```swift
 func contralift<A, B>(_ f: @escaping (B) -> A) -> (Op<Bool, A>) -> Op<Bool, B>
 ```
 ................
 ```Haskell
 a -> a
 ```
 ```swift
 (A) -> A
 ```
 ................
 ```Haskell
 (a -> a) -> f a
 ```
 ```swift
 ((A) -> A) -> Kind<F, A>
 ```
 */

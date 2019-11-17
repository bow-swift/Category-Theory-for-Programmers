/*:
 ```Haskell
 f >=> g = join . fmap g . f
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
infix operator >=>

func >=><F: Monad, A, B, C>(_ f: @escaping (A) -> Kind<F, B>,
                         _ g: @escaping (B) -> Kind<F, C>) -> (A) -> Kind<F, C> {
    F.flatten <<< F.lift(g) <<< f
}

 /*:
 ................
 ```Haskell
 (f >=> g) a = join (fmap g (f a))
 ```
 ```swift
 func >=><F: Monad, A, B, C>(_ f: @escaping (A) -> Kind<F, B>,
                          _ g: @escaping (B) -> Kind<F, C>) -> (A) -> Kind<F, C> {
     { a in f(a).flatMap(g) }
 }
 ```
 ................
 ```Haskell
 class Monoid m where
     mappend :: m -> m -> m
     mempty  :: m
 ```
 */
protocol Monoid {
    func combine(_ other: Self) -> Self
    var empty: Self { get }
}
 /*:
 ................
 ```Haskell
 mappend :: m -> (m -> m)
 ```
 ```swift
 func combine<M>(_ x: M) -> (M) -> M
 ```
 ................
 ```Haskell
 mu :: (m, m) -> m
 ```
 ```swift
 func mu<M>(_ mm: (M, M)) -> M
 ```
 ................
 ```Haskell
 eta :: () -> m
 ```
 ```swift
 func eta<M>() -> M
 ```
 ................
 ```Haskell
 mu (x, mu (y, z)) = mu (mu (x, y), z)
 ```
 ```swift
 mu(x, mu(y, z)) == mu(mu(x, y), z)
 ```
 ................
 ```Haskell
 (mu . bimap id mu)(x, (y, z))
 ```
 ```swift
 (mu <<< bimap(id, mu))(x, (y, z))
 ```
 ................
 ```Haskell
 (mu . bimap mu id)((x, y), z)
 ```
 ```swift
 (mu <<< bimap(mu, id))((x, y), z)
 ```
 ................
 ```Haskell
 mu . bimap id mu = mu . bimap mu id
 ```
 ```swift
 mu <<< bimap(id, mu) == mu <<< bimap(mu, id)
 ```
 ................
 ```Haskell
 alpha :: ((a, b), c) -> (a, (b, c))
 alpha ((x, y), z) = (x, (y, z))
 ```
 */
func alpha<A, B, C>(_ v: ((A, B), C)) -> (A, (B, C)) {
    let ((a, b), c) = v
    return (a, (b, c))
}
 /*:
 ................
 ```Haskell
 mu . bimap id mu . alpha = mu . bimap mu id
 ```
 ```swift
 mu <<< bimap(id, mu) <<< alpha ==
    mu <<< bimap(mu, id)
 ```
 ................
 ```Haskell
 mu (eta (), x) = x
 mu (x, eta ()) = x
 ```
 ```swift
 mu(eta(), x) == x
 mu(x, eta()) == x
 ```
 ................
 ```Haskell
 (mu . bimap eta id) ((), x) = lambda ((), x)
 (mu . bimap id eta) (x, ()) = rho (x, ())
 ```
 ```swift
 (mu <<< bimap(eta, id))(((), x)) ==
   lambda(((), x))

 (mu <<< bimap(id, eta))((x, ())) ==
   rho((x, ()))
 ```
 ................
 ```Haskell
 lambda :: ((), a) -> a
 lambda ((), x) = x
 ```
 */
func lambda<A>(_ v: ((), A)) -> A {
    let (_, x) = v
    return x
}
 /*:
 ................
 ```Haskell
 rho :: (a, ()) -> a
 rho (x, ()) = x
 ```
 */
func rho<A>(_ v: (A, ())) -> A {
    let (x, _) = v
    return x
}
 /*:
 ................
 ```Haskell
 mu . bimap id eta = lambda
 mu . bimap eta id = rho
 ```
 ```swift
 (mu <<< bimap(eta, id)) == lambda
 (mu <<< bimap(id, eta)) == rho
 ```
 ................
 ```Haskell
 newtype State s a = State (s -> (a, s))
 ```
 */
final class ForState {}
final class StatePartial<S>: Kind<ForState, S> {}
final class State<S, A>: Kind<StatePartial<S>, A> {
    let f: (S) -> (A, S)
    
    init(_ f: @escaping (S) -> (A, S)) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<S, A>(_ value: Kind<StatePartial<S>, A>) -> State<S, A> {
    value as! State<S, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 newtype Prod s a = Prod (a, s)
 ```
 */
final class ForProd {}
final class ProdPartial<S>: Kind<ForProd, S> {}
final class Prod<S, A>: Kind<ProdPartial<S>, A> {
    let value: (S, A)
    
    init(_ value: (S, A)) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<S, A>(_ value: Kind<ProdPartial<S>, A>) -> Prod<S, A> {
    value as! Prod<S, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 newtype Reader s a = Reader (s -> a)
 ```
 */
final class ForReader {}
final class ReaderPartial<S>: Kind<ForReader, S> {}
final class Reader<S, A>: Kind<ReaderPartial<S>, A> {
    let f: (S) -> A
    
    init(_ f: @escaping (S) -> A) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<S, A>(_ value: Kind<ReaderPartial<S>, A>) -> Reader<S, A> {
    value as! Reader<S, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 instance Adjunction (Prod s) (Reader s) where
   counit (Prod (Reader f, s)) = f s
   unit a = Reader (\s -> Prod (a, s))
 ```
 */
// nef:begin:hidden
open class Adjunction<F: Functor, U: Functor> {
    init() {}
    
    func unit<A>(_ a: A) -> Kind<U, Kind<F, A>> {
        fatalError("Implement unit in subclasses")
    }
    
    func counit<A>(_ a: Kind<F, Kind<U, A>>) -> A {
        fatalError("Implement counit in subclasses")
    }
}

extension ProdPartial: Functor {
    static func map<A, B>(_ fa: Kind<ProdPartial<S>, A>, _ f: @escaping (A) -> B) -> Kind<ProdPartial<S>, B> {
        Prod((fa^.value.0, f(fa^.value.1)))
    }
}

extension ReaderPartial: Functor {
    static func map<A, B>(_ fa: Kind<ReaderPartial<S>, A>, _ f: @escaping (A) -> B) -> Kind<ReaderPartial<S>, B> {
        Reader { a in f(fa^.f(a)) }
    }
}
class Snippet1 {
// nef:end
class StateAdjunction<S>: Adjunction<ProdPartial<S>, ReaderPartial<S>> {
    override func unit<A>(_ a: A) -> Kind<ReaderPartial<S>, Kind<ProdPartial<S>, A>> {
        Reader { s in Prod((s, a)) }
    }
    
    override func counit<A>(_ fa: Kind<ProdPartial<S>, Kind<ReaderPartial<S>, A>>) -> A {
        let (s, ra) = fa^.value
        return ra^.f(s)
    }
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 newtype State s a = State (s -> (a, s))
 ```
 ```swift
 final class ForState {}
 final class StatePartial<S>: Kind<ForState, S> {}
 final class State<S, A>: Kind<StatePartial<S>, A> {
     let f: (S) -> (A, S)
     
     init(_ f: @escaping (S) -> (A, S)) {
         self.f = f
     }
 }
 ```
 ................
 ```Haskell
 runState :: State s a -> s -> (a, s)
 runState (State f) s = f s
 ```
 */
extension State {
    func run(_ s: S) -> (A, S) {
        f(s)
    }
}
 /*:
 ................
 ```Haskell
 ssa :: State s (State s a)
 runState ssa :: s -> (State s a, s)
 ```
 ```swift
 func ssa<S, A>() -> State<S, State<S, A>>

 func rss<S, A>() -> (S) -> (State<S, A>, S) {
     return runState(ssa).f
 }
 ```
 ................
 ```Haskell
 join :: State s (State s a) -> State s a
 join ssa = State (uncurry runState . runState ssa)
 ```
 */
func flatten<S, A>(_ ssa: State<S, State<S, A>>) -> State<S, A> {
    State<S, A> { s in
        let (sa, s2) = ssa.run(s)
        return sa.run(s2)
    }
}

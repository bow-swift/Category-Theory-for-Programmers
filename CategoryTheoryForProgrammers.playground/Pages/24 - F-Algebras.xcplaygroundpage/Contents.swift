/*:
 ```Haskell
 type Algebra f a = f a -> a
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
typealias Algebra<F, A> = (Kind<F, A>) -> A
 /*:
 ................
 ```Haskell
 data MonF a = MEmpty | MAppend a a
 ```
 */
final class ForMonF {}
final class MonF<A>: Kind<ForMonF, A> {
    enum _MonF<A> {
        case empty
        case append(A, A)
    }
    
    static func empty() -> MonF<A> {
        MonF(.empty)
    }
    
    static func append(_ m: A, _ n: A) -> MonF<A> {
        MonF(.append(m, n))
    }
    
    let value: _MonF<A>
    
    private init(_ value: _MonF<A>) {
        self.value = value
    }
}
 /*:
 ................
 ```Haskell
 data RingF a = RZero
              | ROne
              | RAdd a a
              | RMul a a
              | RNeg a
 ```
 */
final class ForRingF {}
final class RingF<A>: Kind<ForRingF, A> {
    enum _RingF<A> {
        case zero
        case one
        case add(A, A)
        case mul(A, A)
        case neg(A)
    }
    
    static var zero: RingF<A> {
        RingF(.zero)
    }
    
    static var one: RingF<A> {
        RingF(.one)
    }
    
    static func add(_ m: A, _ n: A) -> RingF<A> {
        RingF(.add(m, n))
    }
    
    static func mul(_ m: A, _ n: A) -> RingF<A> {
        RingF(.mul(m, n))
    }
    
    static func neg(_ m: A) -> RingF<A> {
        RingF(.neg(m))
    }
    
    let value: _RingF<A>
    
    private init(_ value: _RingF<A>) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForRingF, A>) -> RingF<A> {
    value as! RingF<A>
}
// nef:end
 /*:
 ................
 ```Haskell
 evalZ :: Algebra RingF Integer
 evalZ RZero      = 0
 evalZ ROne       = 1
 evalZ (RAdd m n) = m + n
 evalZ (RMul m n) = m * n
 evalZ (RNeg n)   = -n
 ```
 */
let evalZ: Algebra<ForRingF, Int> = { fa in
    switch fa^.value {
    case .zero: return 0
    case .one: return 1
    case let .add(m, n): return m + n
    case let .mul(m, n): return m * n
    case let .neg(n): return -n
    }
}
 /*:
 ................
 ```Haskell
 data Expr = RZero
           | ROne
           | RAdd Expr Expr
           | RMul Expr Expr
           | RNeg Expr
 ```
 */
enum Expr {
    case zero
    case one
    indirect case add(Expr, Expr)
    indirect case mul(Expr, Expr)
    indirect case neg(Expr)
}
 /*:
 ................
 ```Haskell
 evalZ :: Expr -> Integer
 evalZ RZero        = 0
 evalZ ROne         = 1
 evalZ (RAdd e1 e2) = evalZ e1 + evalZ e2
 evalZ (RMul e1 e2) = evalZ e1 * evalZ e2
 evalZ (RNeg e)     = -(evalZ e)
 ```
 */
func evalZ(_ expr: Expr) -> Int {
    switch(expr) {
    case .zero: return 0
    case .one: return 1
    case let .add(e1, e2): return evalZ(e1) + evalZ(e2)
    case let .mul(e1, e2): return evalZ(e1) * evalZ(e2)
    case let .neg(e): return -evalZ(e)
    }
}
 /*:
 ................
 ```Haskell
 type RingF1 a = RingF (RingF a)
 ```
 ```swift
 typealias RingF1<A> = RingF<RingF<A>>
 ```
 ................
 ```Haskell
 type RingF2 a = RingF (RingF (RingF a))
 ```
 ```swift
 typealias RingF2<A> = RingF<RingF<RingF<A>>>
 ```
 ................
 ```Haskell
 type RingF2 a = RingF (RingF1 a)
 ```
 ```swift
 typealias RingF2<A> = RingF<RingF1<A>>
 ```
 ................
 ```Haskell
 type RingFn+1 a = RingF (RingFn a)
 ```
 ```swift
 typealias RingFnplus1<A> = RingF<RingFn<A>>
 ```
 ................
 ```Haskell
 newtype Fix f = Fix (f (Fix f))
 ```
 */
final class ForFix {}
final class Fix<F>: Kind<ForFix, F> {
    let unFix: Kind<F, Fix<F>>
    
    init(unFix: Kind<F, Fix<F>>) {
        self.unFix = unFix
    }
}
// nef:begin:hidden
postfix func ^<F>(_ value: Kind<ForFix, F>) -> Fix<F> {
    value as! Fix<F>
}
// nef:end
 /*:
 ................
 ```Haskell
 newtype Fix f = In (f (Fix f))
 ```
 ```swift
 final class ForFix {}
 final class Fix<F>: Kind<ForFix, F> {
     let unFix: Kind<F, Kind<ForFix, F>>
     
     init(unFix: Kind<F, Fix<F>>) {
         self.unFix = unFix
     }
 }
 ```
 ................
 ```Haskell
 Fix :: f (Fix f) -> Fix f
 ```
 ```swift
 Fix.init: (Kind<F, Kind<ForFix, F>>) -> Fix<F>
 ```
 ................
 ```Haskell
 unFix :: Fix f -> f (Fix f)
 unFix (Fix x) = x
 ```
 ```swift
 fix.unfix
 ```
 ................
 ```Haskell
 data NatF a = ZeroF | SuccF a
 ```
 */
final class ForNatF {}
final class NatF<A>: Kind<ForNatF, A> {
    enum _NatF<A> {
        case zero
        case succ(A)
    }
    
    static var zero: NatF<A> {
        NatF(.zero)
    }
    
    static func succ(_ a: A) -> NatF<A> {
        NatF(.succ(a))
    }
    
    let value: _NatF<A>
    
    private init(_ value: _NatF<A>) {
        self.value = value
    }
}
 /*:
 ................
 ```Haskell
 data Nat = Zero | Succ Nat
 ```
 ```swift
 enum Nat {
     case zero
     case succ(Nat)
 }
 ```
 ................
 ```Haskell
 cata :: Functor f => (f a -> a) -> Fix f -> a
 cata alg = alg . fmap (cata alg) . unFix
 ```
 */
func cata<F: Functor, A>(_ alg: @escaping Algebra<F, A>) -> (Fix<F>) -> A {
    alg <<< F.lift(cata(alg)) <<< { fix in fix.unFix }
}
 /*:
 ................
 ```Haskell
 data NatF a = ZeroF | SuccF a
 ```
 ```swift
 final class ForNatF {}
 final class NatF<A>: Kind<ForNatF, A> {
     enum _NatF<A> {
         case zero
         case succ(A)
     }
     
     static var zero: NatF<A> {
         NatF(.zero)
     }
     
     static func succ(_ a: A) -> NatF<A> {
         NatF(.succ(a))
     }
     
     let value: _NatF<A>
     
     private init(_ value: _NatF<A>) {
         self.value = value
     }
 }
 ```
 ................
 ```Haskell
 fib :: NatF (Int, Int) -> (Int, Int)
 fib ZeroF = (1, 1)
 fib (SuccF (m, n)) = (n, m + n)
 ```
 */
func fib(_ fa: NatF<(Int, Int)>) -> (Int, Int) {
    switch fa.value {
    case .zero: return (1, 1)
    case let .succ(m, n): return (n, m + n)
    }
}
/*:
 ................
 ```Haskell
 data ListF e a = NilF | ConsF e a
 ```
 */
final class ForListF {}
final class ListFPartial<E>: Kind<ForListF, E> {}
final class ListF<E, A>: Kind<ListFPartial<E>, A> {
    enum _ListF<E, A> {
        case nilF
        case consF(E, A)
    }
    
    static var nilF: ListF<E, A> {
        ListF(.nilF)
    }
    
    static func consF(_ e: E, _ a: A) -> ListF<E, A> {
        ListF(.consF(e, a))
    }
    
    let value: _ListF<E, A>
    
    private init(_ value: _ListF<E, A>) {
        self.value = value
    }
}
 /*:
 ................
 ```Haskell
 data List e = Nil | Cons e (List e)
 ```
 */
enum List<E> {
    case `nil`
    indirect case cons(E, List<E>)
}
 /*:
 ................
 ```Haskell
 lenAlg :: ListF e Int -> Int
 lenAlg (ConsF e n) = n + 1
 lenAlg NilF = 0
 ```
 */
func lenAlg<E>(_ list: ListF<E, Int>) -> Int {
    switch list.value {
    case .nilF: return 0
    case let .consF(_, n): return n + 1
    }
}
 /*:
 ................
 ```Haskell
 length = foldr (\e n -> n + 1) 0
 ```
 */
func length<E>(_ array: [E]) -> Int {
    array.reduce(0) { n, _ in n + 1 }
}
 /*:
 ................
 ```Haskell
 sumAlg :: ListF Double Double -> Double
 sumAlg (ConsF e s) = e + s
 sumAlg NilF = 0.0
 ```
 */
func sumAlg(_ list: ListF<Double, Double>) -> Double {
    switch list.value {
    case .nilF: return 0.0
    case let .consF(e, s): return e + s
    }
}
 /*:
 ................
 ```Haskell
 sum = foldr (\e s -> e + s) 0.0
 ```
 */
func sum(_ array: [Double]) -> Double {
    array.reduce(0.0) { s, e in e + s }
}
 /*:
 ................
 ```Haskell
 ana :: Functor f => (a -> f a) -> a -> Fix f
 ana coalg = Fix . fmap (ana coalg) . coalg
 ```
 */
typealias CoAlgebra<F, A> = (A) -> Kind<F, A>

func ana<F: Functor, A>(_ coalg: @escaping CoAlgebra<F, A>) -> (A) -> Fix<F> {
    Fix.init <<< F.lift(ana(coalg)) <<< coalg
}
 /*:
 ................
 ```Haskell
 data StreamF e a = StreamF e a
   deriving Functor
 ```
 */
final class ForStreamF {}
final class StreamFPartial<E>: Kind<ForStreamF, E> {}
final class StreamF<E, A>: Kind<StreamFPartial<E>, A> {
    let head: () -> E
    let tail: () -> A
    
    init(_ head: @escaping () -> E, _ tail: @escaping () -> A) {
        self.head = head
        self.tail = tail
    }
}
// nef:begin:hidden
postfix func ^<E, A>(_ value: Kind<StreamFPartial<E>, A>) -> StreamF<E, A> {
    value as! StreamF<E, A>
}
// nef:end
extension StreamFPartial: Functor {
    static func map<A, B>(_ fa: Kind<StreamFPartial<E>, A>, _ f: @escaping (A) -> B) -> Kind<StreamFPartial<E>, B> {
        StreamF(fa^.head, { f(fa^.tail()) })
    }
}
 /*:
 ................
 ```Haskell
 data Stream e = Stream e (Stream e)
 ```
 */
final class ForStream {}
final class Stream<E>: Kind<ForStream, E> {
    let head: () -> E
    let tail: () -> Stream<E>
    
    init(_ head: @escaping () -> E, _ tail: @escaping () -> Stream<E>) {
        self.head = head
        self.tail = tail
    }
}
 /*:
 ................
 ```Haskell
 era :: [Int] -> StreamF Int [Int]
 era (p : ns) = StreamF p (filter (notdiv p) ns)
     where notdiv p n = n `mod` p /= 0
 ```
 */
func era(_ list: [Int]) -> StreamF<Int, [Int]> {
    func notdiv(_ p: Int, _ n: Int) -> Bool {
        n % p != 0
    }
    let p = list[0]
    let ns = Array(list.dropFirst())
    return StreamF({ p }, { ns.filter { x in notdiv(p, x) } })
}
 /*:
 ................
 ```Haskell
 primes = ana era [2..]
 ```
 */
func primes(_ array: [Int]) -> Fix<StreamFPartial<Int>> {
    ana(era)(array)
}
 /*:
 ................
 ```Haskell
 toListC :: Fix (StreamF e) -> [e]
 toListC = cata al
    where al :: StreamF e [e] -> [e]
          al (StreamF e a) = e : a
 ```
 */
func toListC<E>(_ fix: Fix<StreamFPartial<E>>) -> [E] {
    func al(_ stream: Kind<StreamFPartial<E>, [E]>) -> [E] {
        [stream^.head()] + stream^.tail()
    }
    return cata(al)(fix)
}
 /*:
 ................
 ```Haskell
 unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
 ```
 ```swift
 func unfoldr<A, B>(_ f: @escaping (B) -> Option<(A, B)>, _ b: B) -> [A]
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
 ................
 ```Haskell
 a -> (s, s -> a)
 ```
 ```swift
 (A) -> (S, (S) -> A)
 ```
 ................
 ```Haskell
 a -> Store s a
 ```
 ```swift
 (A) -> Store<S, A>
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

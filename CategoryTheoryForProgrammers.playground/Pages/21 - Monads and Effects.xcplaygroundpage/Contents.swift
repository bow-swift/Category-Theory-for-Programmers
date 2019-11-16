/*:
 ................
 ```Haskell
 instance Monad [] where
     join = concat
     return x = [x]
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
final class ForList {}
final class List<A>: Kind<ForList, A> {
    let values: [A]
    
    init(_ values: [A]) {
        self.values = values
    }
    
    init(_ values: A...) {
        self.values = values
    }
}
// nef:begin:hidden
postfix func ^<A>(_ value: Kind<ForList, A>) -> List<A> {
    value as! List<A>
}

extension ForList: Functor {
    static func map<A, B>(_ fa: Kind<ForList, A>, _ f: @escaping (A) -> B) -> Kind<ForList, B> {
        List(fa^.values.map(f))
    }
}
// nef:end

extension ForList: Monad {
    static func pure<A>(_ a: A) -> Kind<ForList, A> {
        List(a)
    }
    
    static func flatMap<A, B>(_ fa: Kind<ForList, A>, _ f: @escaping (A) -> Kind<ForList, B>) -> Kind<ForList, B> {
        List(fa^.values.flatMap { a in f(a)^.values })
    }
    // nef:begin:hidden
    static func tailRecM<A, B>(_ a: A, _ f : @escaping (A) -> Kind<ForList, Either<A, B>>) -> Kind<ForList, B> {
        fatalError()
    }
    // nef:end
}

 /*:
 ................
 ```Haskell
 as >>= k = concat (fmap k as)
 ```
 ```swift
 func flatMap<A, B>(_ fa: Kind<ForList, A>, _ f: @escaping (A) -> Kind<ForList, B>) -> Kind<ForList, B> {
     List(fa^.values.flatMap { a in f(a)^.values })
 }
 ```
 ................
 ```Haskell
 guard :: Bool -> [()]
 guard True  = [()]
 guard False = []
 ```
 */
func `guard`(_ b: Bool) -> List<Void> {
    b ? List(()) : List()
}
 /*:
 ................
 ```Haskell
 triples = do
     z <- [1..]
     x <- [1..z]
     y <- [x..z]
     guard (x^2 + y^2 == z^2)
     return (x, y, z)
 ```
 */
 // Assuming that we can create a lazy infinite list in Swift...
extension List where A == Int {
    convenience init(range: ClosedRange<Int>) {
        self.init(range.map(id))
    }
}

func triples() -> List<(Int, Int, Int)> {
    let z = List<Int>.var()
    let x = List<Int>.var()
    let y = List<Int>.var()
    
    return binding(
        z <- List(range: 1...100),
        x <- List(range: 1...z.get),
        y <- List(range: x.get...z.get),
          |<-`guard`(x.get * x.get + y.get * y.get == z.get * z.get),
        yield: (x.get, y.get, z.get))^
}
 /*:
 ................
 ```Haskell
 triples = [(x, y, z) | z <- [1..]
                      , x <- [1..z]
                      , y <- [x..z]
                      , x^2 + y^2 == z^2]
 ```
 ```swift
 func triples() -> List<(Int, Int, Int)> {
     let z = List<Int>.var()
     let x = List<Int>.var()
     let y = List<Int>.var()
     
     return binding(
         z <- List(range: 1...100),
         x <- List(range: 1...z.get),
         y <- List(range: x.get...z.get),
           |<-`guard`(x.get * x.get + y.get * y.get == z.get * z.get),
         yield: (x.get, y.get, z.get))^
 }
 ```
 ................
 ```Haskell
 newtype Reader e a = Reader (e -> a)
 ```
 */
final class ForReader {}
final class ReaderPartial<E>: Kind<ForReader, E> {}
final class Reader<E, A>: Kind<ReaderPartial<E>, A> {
    let f: (E) -> A
    
    init(_ f: @escaping (E) -> A) {
        self.f = f
    }
}
// nef:begin:hidden
postfix func ^<E, A>(_ value: Kind<ReaderPartial<E>, A>) -> Reader<E, A> {
    value as! Reader<E, A>
}
//
 /*:
 ................
 ```Haskell
 runReader :: Reader e a -> e -> a
 runReader (Reader f) e = f e
 ```
 */
extension Reader {
    func run(_ e: E) -> A {
        self.f(e)
    }
}
 /*:
 ................
 ```Haskell
 ra >>= k = Reader (\e -> ...)
 ```
 ```swift
func flatMap<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> Kind<ReaderPartial<E>, B>) -> Kind<ReaderPartial<E>, B> {
    Reader<E, B>({ e in ... })
}
 ```
 ................
 ```Haskell
 ra >>= k = Reader (\e -> let a = runReader ra e
                          in ...)
 ```
 ```swift
func flatMap<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> Kind<ReaderPartial<E>, B>) -> Kind<ReaderPartial<E>, B> {
    Reader<E, B>({ e in
        let a = fa^.run(e)
        ...
    })
}
 ```
 ................
 ```Haskell
 ra >>= k = Reader (\e -> let a  = runReader ra e
                              rb = k a
                          in ...)
 ```
 ```swift
func flatMap<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> Kind<ReaderPartial<E>, B>) -> Kind<ReaderPartial<E>, B> {
        Reader<E, B>({ e in
        let a = fa^.run(e)
        let rb = f(a)
        ...
    })
}
 ```
 ................
 ```Haskell
 ra >>= k = Reader (\e -> let a  = runReader ra e
                              rb = k a
                          in runReader rb e)
 ```
 ```swift
func flatMap<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> Kind<ReaderPartial<E>, B>) -> Kind<ReaderPartial<E>, B> {
    Reader<E, B>({ e in
        let a = fa^.run(e)
        let rb = f(a)
        return rb^.run(e)
    })
}
 ```
 ................
 ```Haskell
 instance Monad (Reader e) where
     ra >>= k = Reader (\e -> runReader (k (runReader ra e)) e)
     return x = Reader (\e -> x)
 ```
 */
// nef:begin:hidden
extension ReaderPartial: Functor {
    static func map<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> B) -> Kind<ReaderPartial<E>, B> {
        Reader(fa^.f >>> f)
    }
}
// nef:end
extension ReaderPartial: Monad {
    static func pure<A>(_ a: A) -> Kind<ReaderPartial<E>, A> {
        Reader { _ in a }
    }
    
    static func flatMap<A, B>(_ fa: Kind<ReaderPartial<E>, A>, _ f: @escaping (A) -> Kind<ReaderPartial<E>, B>) -> Kind<ReaderPartial<E>, B> {
        Reader<E, B>({ e in
            let a = fa^.run(e)
            let rb = f(a)
            return rb^.run(e)
        })
    }
    // nef:begin:hidden
    static func tailRecM<A, B>(_ a: A, _ f: @escaping (A) -> Kind<ReaderPartial<E>, Either<A, B>>) -> Kind<ReaderPartial<E>, B> {
        fatalError()
    }
    // nef:end
}
 /*:
 ................
 ```Haskell
 newtype Writer w a = Writer (a, w)
 ```
 */
final class ForWriter {}
final class WriterPartial<W>: Kind<ForWriter, W> {}
final class Writer<W, A>: Kind<WriterPartial<W>, A> {
    let value: (W, A)
    
    init(_ value: (W, A)) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<W, A>(_ value: Kind<WriterPartial<W>, A>) -> Writer<W, A> {
    value as! Writer<W, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 runWriter :: Writer w a -> (a, w)
 runWriter (Writer (a, w)) = (a, w)
 ```
 */
extension Writer {
    func run() -> (W, A) {
        value
    }
}
 /*:
 ................
 ```Haskell
 instance (Monoid w) => Monad (Writer w) where
     (Writer (a, w)) >>= k = let (a', w') = runWriter (k a)
                             in Writer (a', w `mappend` w')
     return a = Writer (a, mempty)
 ```
 */
// nef:begin:hidden
extension WriterPartial: Functor {
    static func map<A, B>(_ fa: Kind<WriterPartial<W>, A>, _ f: @escaping (A) -> B) -> Kind<WriterPartial<W>, B> {
        Writer((fa^.run().0, f(fa^.run().1)))
    }
}
extension WriterPartial: Applicative where W: Monoid {}
extension WriterPartial: Selective where W: Monoid {}
// nef:end
extension WriterPartial: Monad where W: Monoid {
    static func pure<A>(_ a: A) -> Kind<WriterPartial<W>, A> {
        Writer((W.empty(), a))
    }
    
    static func flatMap<A, B>(_ fa: Kind<WriterPartial<W>, A>, _ f: @escaping (A) -> Kind<WriterPartial<W>, B>) -> Kind<WriterPartial<W>, B> {
        let (w, a) = fa^.run()
        let (w2, b) = f(a)^.run()
        return Writer((w.combine(w2), b))
    }
    
    // nef:begin:hidden
    static func tailRecM<A, B>(_ a: A, _ f: @escaping (A) -> Kind<WriterPartial<W>, Either<A, B>>) -> Kind<WriterPartial<W>, B> {
        fatalError()
    }
    // nef:end
}
 /*:
 ................
 ```Haskell
 newtype State s a = State (s -> (a, s))
 ```
 */
final class ForState {}
final class StatePartial<S>: Kind<ForState, S> {}
final class State<S, A>: Kind<StatePartial<S>, A> {
    let f: (S) -> (S, A)
    
    init(_ f: @escaping (S) -> (S, A)) {
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
 runState :: State s a -> s -> (a, s)
 runState (State f) s = f s
 ```
 */
extension State {
    func run(_ s: S) -> (S, A) {
        f(s)
    }
}
 /*:
 ................
 ```Haskell
 sa >>= k = State (\s -> let (a, s') = runState sa s
                             sb = k a
                         in runState sb s')
 ```
 ```swift
 static func flatMap<A, B>(_ fa: Kind<StatePartial<S>, A>, _ f: @escaping (A) -> Kind<StatePartial<S>, B>) -> Kind<StatePartial<S>, B> {
     State { s in
         let (s2, a) = fa^.run(s)
         return f(a)^.run(s2)
     }
 }
 ```
 ................
 ```Haskell
 instance Monad (State s) where
     sa >>= k = State (\s -> let (a, s') = runState sa s
                             in runState (k a) s')
     return a = State (\s -> (a, s))
 ```
 */
 // nef:begin:hidden
extension StatePartial: Functor {
    static func map<A, B>(_ fa: Kind<StatePartial<S>, A>, _ f: @escaping (A) -> B) -> Kind<StatePartial<S>, B> {
        State { s in
            let (s2, a) = fa^.run(s)
            return (s2, f(a))
        }
    }
}
 // nef:end
extension StatePartial: Monad {
    static func pure<A>(_ a: A) -> Kind<StatePartial<S>, A> {
        State { s in (s, a) }
    }
    
    static func flatMap<A, B>(_ fa: Kind<StatePartial<S>, A>, _ f: @escaping (A) -> Kind<StatePartial<S>, B>) -> Kind<StatePartial<S>, B> {
        State { s in
            let (s2, a) = fa^.run(s)
            return f(a)^.run(s2)
        }
    }
    // nef:begin:hidden
    static func tailRecM<A, B>(_ a: A, _ f: @escaping (A) -> Kind<StatePartial<S>, Either<A, B>>) -> Kind<StatePartial<S>, B> {
        fatalError()
    }
    // nef:end
}
 /*:
 ................
 ```Haskell
 get :: State s s
 get = State (\s -> (s, s))
 ```
 */
extension State {
    static func get() -> State<S, S> {
        State<S, S> { s in (s, s) }
    }
}
 /*:
 ................
 ```Haskell
 put :: s -> State s ()
 put s' = State (\s -> ((), s'))
 ```
 */
extension State {
    static func put(_ s: S) -> State<S, Void> {
        State<S, Void> { _ in (s, ()) }
    }
}
 /*:
 ................
 ```Haskell
 instance Monad Maybe where
     Nothing >>= k = Nothing
     Just a  >>= k = k a
     return a = Just a
 ```
 */
// nef:begin:hidden
final class ForOption {}
final class Option<A>: Kind<ForOption, A> {
    enum _Option<A> {
        case none
        case some(A)
    }
    
    static func none() -> Option<A> {
        Option(.none)
    }
    
    static func some(_ a: A) -> Option<A> {
        Option(.some(a))
    }
    
    let value: _Option<A>
    
    private init(_ value: _Option<A>) {
        self.value = value
    }
}

postfix func ^<A>(_ value: Kind<ForOption, A>) -> Option<A> {
    value as! Option<A>
}

extension ForOption: Functor {
    static func map<A, B>(_ fa: Kind<ForOption, A>, _ f: @escaping (A) -> B) -> Kind<ForOption, B> {
        switch fa^.value {
        case .none: return Option.none()
        case let .some(a): return Option.some(f(a))
        }
    }
}
// nef:end
extension ForOption: Monad {
    static func pure<A>(_ a: A) -> Kind<ForOption, A> {
        Option.some(a)
    }
    
    static func flatMap<A, B>(_ fa: Kind<ForOption, A>, _ f: @escaping (A) -> Kind<ForOption, B>) -> Kind<ForOption, B> {
        switch fa^.value {
        case .none: return Option.none()
        case let .some(a): return f(a)
        }
    }
    // nef:begin:hidden
    static func tailRecM<A, B>(_ a: A, _ f: @escaping (A) -> Kind<ForOption, Either<A, B>>) -> Kind<ForOption, B> {
        fatalError()
    }
    // nef:end
}
 /*:
 ................
 ```Haskell
 data Cont r a = Cont ((a -> r) -> r)
 ```
 */
final class ForCont {}
final class ContPartial<R>: Kind<ForCont, R> {}
final class Cont<R, A>: Kind<ContPartial<R>, A> {
    let cont: ((A) -> R) -> R
    
    init(_ cont: @escaping ((A) -> R) -> R) {
        self.cont = cont
    }
}
// nef:begin:hidden
postfix func ^<R, A>(_ value: Kind<ContPartial<R>, A>) -> Cont<R, A> {
    value as! Cont<R, A>
}
// nef:end
 /*:
 ................
 ```Haskell
 runCont :: Cont r a -> (a -> r) -> r
 runCont (Cont k) h = k h
 ```
 */
extension Cont {
    func run(_ k: (A) -> R) -> R {
        cont(k)
    }
}
 /*:
 ................
 ```Haskell
 (>>=) :: ((a -> r) -> r) ->
          (a -> (b -> r) -> r) ->
          ((b -> r) -> r)
 ```
 ```swift
 static func flatMap<A, B>(_ fa: Kind<ContPartial<R>, A>, _ f: @escaping (A) -> Kind<ContPartial<R>, B>) -> Kind<ContPartial<R>, B>

 // Equivalent to:
 // func flatmap<A, B>(_ fa: ((A) -> R) -> R, _ f: @escaping (A) -> (((B) -> R) -> R)) -> ((B) -> R) -> R
 ```
 ................
 ```Haskell
 ka >>= kab = Cont (\hb -> ...)
 ```
 ```swift
 static func flatMap<A, B>(_ ka: Kind<ContPartial<R>, A>, _ kab: @escaping (A) -> Kind<ContPartial<R>, B>) -> Kind<ContPartial<R>, B> {
   return Cont{ hb in
     ...
   }
 }
 ```
 ................
 ```Haskell
 runCont ka (\a -> ...)
 ```
 ```swift
 ka^.run { a in ... }
 ```
 ................
 ```Haskell
 runCont ka (\a -> let kb = kab a
                   in runCont kb hb)
 ```
 ```swift
 ka^.run { a in
   let kb = kab(a)
   kb^.run(hb)
 }
 ```
 ................
 ```Haskell
 instance Monad (Cont r) where
     ka >>= kab = Cont (\hb -> runCont ka (\a -> runCont (kab a) hb))
     return a = Cont (\ha -> ha a)
 ```
 */
// nef:begin:hidden
extension ContPartial: Functor {
    static func map<A, B>(_ fa: Kind<ContPartial<R>, A>, _ f: @escaping (A) -> B) -> Kind<ContPartial<R>, B> {
        Cont { kb in
            fa^.run { a in kb(f(a)) }
        }
    }
}
// nef:end
extension ContPartial: Monad {
    static func pure<A>(_ a: A) -> Kind<ContPartial<R>, A> {
        Cont { ha in ha(a) }
    }
    
    static func flatMap<A, B>(_ ka: Kind<ContPartial<R>, A>, _ kab: @escaping (A) -> Kind<ContPartial<R>, B>) -> Kind<ContPartial<R>, B> {
        Cont { hb in
            ka^.run { a in
                kab(a)^.run(hb)
            }
        }
    }
    // nef:begin:hidden
    static func tailRecM<A, B>(_ a: A, _ f: @escaping (A) -> Kind<ContPartial<R>, Either<A, B>>) -> Kind<ContPartial<R>, B> {
        fatalError()
    }
    // nef:end
}
 /*:
 ................
 ```Haskell
 getChar :: () -> IO Char
 ```
 ```swift
 func getChar() -> IO<Error, Character>
 ```
 ................
 ```Haskell
 main :: IO ()
 ```
 ```swift
 func main() -> IO<Error, Void>
 ```
 ................
 ```Haskell
 main :: () -> IO ()
 ```
 ```swift
 func main() -> IO<Error, Void>
 ```
 ................
 ```Haskell
 type IO a  =  RealWorld -> (a, RealWorld)
 ```
 ```swift
 typealias IO<A> = (RealWorld) -> (A, RealWorld)
 ```
 ................
 ```Haskell
 type IO = State RealWorld
 ```
 ```swift
 typealias IO<A> = State<RealWorld, A>
 ```
 ................
 ```Haskell
 putStr :: String -> IO ()
 ```
 ```swift
 func putStr(_ str: String) -> IO<Error, Void>
 ```
 ................
 ```Haskell
 putStr :: String -> ()
 ```
 ```swift
 func putStr(_ str: String) -> ()
 ```
 ................
 ```Haskell
 main :: IO ()
 main = do
     putStr "Hello "
     putStr "World!"
 ```
 */
// nef:begin:hidden
import BowEffects
// nef:end
func main() -> IO<Error, Void> {
    binding(
        |<-ConsoleIO.print("Hello"),
        |<-ConsoleIO.print("World!"),
        yield: ())^
}

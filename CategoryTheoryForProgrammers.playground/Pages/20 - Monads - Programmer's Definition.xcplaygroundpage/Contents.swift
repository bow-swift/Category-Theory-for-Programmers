/*:
 ................
 ```Haskell
 vlen = sqrt . sum . fmap (flip (^) 2)
 ```
 */
// nef:begin:hidden
import Bow
import Foundation

func sum(_ list: ArrayKOf<Double>) -> Double {
    list.combineAll()
}
// nef:end
func vlen(_ list: ArrayK<Double>) -> Double {
    (sqrt <<< sum <<< ArrayK<Double>.lift { x in pow(x, 2) })(list)
}
 /*:
 ................
 ```Haskell
 newtype Writer w a = Writer (a, w)

 instance Functor (Writer w) where
   fmap f (Writer (a, w)) = Writer (f a, w)
 ```
 */
final class ForWriter {}
final class WriterPartial<W>: Kind<ForWriter, W> {}
class Writer<W, A>: Kind<WriterPartial<W>, A> {
    let value: (W, A)
    
    init(_ value: (W, A)) {
        self.value = value
    }
}
// nef:begin:hidden
postfix func ^<W, A>(_ value: Kind<WriterPartial<W>, A>) -> Writer<W, A> {
    value as! Writer<W, A>
}
// nef:end
extension WriterPartial: Functor {
    static func map<A, B>(_ fa: Kind<WriterPartial<W>, A>, _ f: @escaping (A) -> B) -> Kind<WriterPartial<W>, B> {
        let (w, a) = fa^.value
        return Writer((w, f(a)))
    }
}
 /*:
 ................
 ```Haskell
 a -> Writer w b
 ```
 ```swift
 (A) -> Writer<W, B>
 ```
 ................
 ```Haskell
 class Monad m where
   (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
   return :: a -> m a
 ```
 */
protocol Monad {
    static func andThen<A, B, C>(_ f: @escaping (A) -> Kind<Self, B>, _ g: @escaping (B) -> Kind<Self, C>) -> (A) -> Kind<Self, C>
    static func pure<A>(_ a: A) -> Kind<Self, A>
}
 /*:
 ................
 ```Haskell
 instance Monoid w => Monad (Writer w) where
     f >=> g = \a ->
         let Writer (b, s)  = f a
             Writer (c, s') = g b
         in Writer (c, s `mappend` s')
     return a = Writer (a, mempty)
 ```
 */
extension WriterPartial: Monad where W: Monoid {
    static func andThen<A, B, C>(_ f: @escaping (A) -> Kind<WriterPartial<W>, B>, _ g: @escaping (B) -> Kind<WriterPartial<W>, C>) -> (A) -> Kind<WriterPartial<W>, C> {
        { a in
            let (s, b) = f(a)^.value
            let (s2, c) = g(b)^.value
            return Writer((s.combine(s2), c))
        }
    }
    
    static func pure<A>(_ a: A) -> Kind<WriterPartial<W>, A> {
        Writer((W.empty(), a))
    }
}
 /*:
 ................
 ```Haskell
 tell :: w -> Writer w ()
 tell s = Writer ((), s)
 ```
 */
func tell<W>(_ s: W) -> Writer<W, Void> {
    Writer((s, ()))
}
 /*:
 ................
 ```Haskell
 (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
 f >=> g = \a -> ...
 ```
 ```swift
func andThen<A, B, C, M>(_ f: @escaping (A) -> Kind<M, B>, _ g: @escaping (B) -> Kind<M, C>) -> (A) -> Kind<M, C> {
    { a in ... }
}
 ```
 ................
 ```Haskell
 f >=> g = \a -> let mb = f a
                 in ...
 ```
 ```swift
func andThen<A, B, C, M>(_ f: @escaping (A) -> Kind<M, B>, _ g: @escaping (B) -> Kind<M, C>) -> (A) -> Kind<M, C> {
    { a in
        let mb = f(a)
        ...
    }
}
 ```
 ................
 ```Haskell
 (>>=) :: m a -> (a -> m b) -> m b
 ```
 ```swift
 func flatMap<A, B>(_ ma: Kind<M, A>, _ f: @escaping (A) -> Kind<M, B>) -> Kind<M, B>
 ```
 ................
 ```Haskell
 class Monad m where
     (>>=) :: m a -> (a -> m b) -> m b
     return :: a -> m a
 ```
 ```swift
protocol Monad {
   static func flatMap<A, B>(_ ma: Kind<Self, A>, _ f: @escaping (A) -> Kind<Self, B>) -> Kind<Self, B>
   static func pure<A>(_ a: A) -> Kind<Self, A>
}
 ```
 ................
 ```Haskell
 (Writer (a, w)) >>= f = let Writer (b, w') = f a
                         in Writer (b, w `mappend` w')
 ```
 ```swift
extension WriterPartial: Monad where W: Monoid {
    static func flatMap<A, B>(_ ma: Kind<WriterPartial<W>, A>, _ f: @escaping (A) -> Kind<WriterPartial<W>, B>) -> Kind<WriterPartial<W>, B> {
        let (s, a) = ma^.value
        let (s2, b) = f(a)^.value
        return Writer((b, s.combine(s2)))
    }
    
    static func pure(_ a: A) -> Kind<WriterPartial<W>, A> {
        Writer((W.empty(), a))
    }
}
 ```
 ................
 ```Haskell
 join :: m (m a) -> m a
 ```
 ```swift
 func flatten<A, M>(_ mma: Kind<M, Kind<M, A>>) -> Kind<M, A>
 ```
 ................
 ```Haskell
 ma >>= f = join (fmap f ma)
 ```
 */
// nef:begin:hidden
func flatten<M: Monad, A>(_ mma: Kind<M, Kind<M, A>>) -> Kind<M, A> {
    fatalError()
}
func map<M, A, B>(_ ma: Kind<M, A>, _ f: @escaping (A) -> B) -> Kind<M, B> {
    fatalError()
}
// nef:end
func flatMap<M: Monad, A, B>(_ ma: Kind<M, A>, _ f: @escaping (A) -> Kind<M, B>) -> Kind<M, B> {
    flatten(map(ma, f))
}
 /*:
 ................
 ```Haskell
 class Functor m => Monad m where
     join :: m (m a) -> m a
     return :: a -> m a
 ```
 ```swift
protocol Monad: Functor {
    static func flatten<A>(_ ffa: Kind<Self, Kind<Self, A>>) -> Kind<Self, A>
    static func pure<A>(_ a: A) -> Kind<F, A>
}
 ```
 ................
 ```Haskell
 fmap f ma = ma >>= \a -> return (f a)
 ```
 ```swift
func map<F: Monad, A, B>(_ fa: Kind<F, A>, _ f: @escaping (A) -> B) -> Kind<F, B> {
    F.flatMap(ma) { a in F.pure(f(a)) }
}
 ```
 ................
 ```Haskell
 join :: Monoid w => Writer w (Writer w a) -> Writer w a
 join (Writer ((Writer (a, w')), w)) = Writer (a, w `mappend` w')
 ```
 */
func flatten<A, W: Monoid>(_ wwa: Writer<W, Writer<W, A>>) -> Writer<W, A> {
    let (w, inner) = wwa.value
    let (w2, a) = inner.value
    return Writer((w.combine(w2), a))
}
 /*:
 ................
 ```Haskell
 upCase :: String -> Writer String String
 upCase s = Writer (map toUpper s, "upCase ")
 ```
 */
func upCase(_ s: String) -> Writer<String, String> {
    Writer(("upCase ", s.uppercased()))
}
 /*:
 ................
 ```Haskell
 process :: String -> Writer String [String]
 process = upCase >=> toWords
 ```
 */
func toWords(_ s: String) -> Writer<String, [String]> {
    Writer(("toWords ", s.components(separatedBy: " ")))
}

let process = WriterPartial<String>.andThen(upCase, toWords)
 /*:
 ................
 ```Haskell
 process s = do
     upStr <- upCase s
     toWords upStr
 ```
 ```swift
func process(_ s: String) -> Writer<String, [String]> {
    let upStr = Writer<String, String>.var()
    let words = Writer<String, [String]>.var()
    
    return binding(
        upStr <- upCase(s),
        words <- toWords(upStr.get),
        yield: words.get)^
}
 ```
 ................
 ```Haskell
 process s =
    upCase s >>= \ upStr ->
        toWords upStr
 ```
 ```swift
let process: (String) -> Writer<String, [String]> = { s in
    upCase(s).flatMap { upStr in toWords(upStr) }^
}
 ```
 ................
 ```Haskell
 upStr <- upCase s
 ```
 ```swift
 upStr <- upCase(s)
 ```
 ................
 ```Haskell
 process s = do
     upStr <- upCase s
     tell "toWords "
     return (words upStr)
 ```
 ```swift
func words(_ s: String) -> [String] {
    s.components(separatedBy: " ")
}

func process(_ s: String) -> Writer<String, [String]> {
    let upStr = Writer<String, String>.var()

    return binding(
        upStr <- upCase(s),
              |<-tell("toWords "),
        yield: words(upStr.get))^
}
 ```
 ................
 ```Haskell
 process s =
     upCase s >>= \upStr ->
       tell "toWords " >>= \() ->
         return (words upStr)
 ```
 ```swift
let process: (String) -> Writer<String, [String]> = { s in
    upCase(s).flatMap { upStr in
        tell("toWords ").flatMap { _ in
            WriterPartial<String>.pure(words(upStr))
        }
    }^
}
 ```
 ................
 ```Haskell
 (>>) :: m a -> m b -> m b
 m >> k = m >>= (\_ -> k)
 ```
 ```
func followedBy<F: Monad, A, B>(_ ma: Kind<F, A>, _ mb: Kind<F, B>) -> Kind<F, B> {
    ma.flatMap { _ in mb }
}
 ```
 ................
 ```Haskell
 process s =
     upCase s >>= \upStr ->
       tell "toWords " >>
         return (words upStr)
 ```
 ```swift
let process: (String) -> Writer<String, [String]> = { s in
    upCase(s).flatMap { upStr in
        tell("toWords ").followedBy(WriterPartial<String>.pure(words(upStr)))
    }
}
 ```
 */

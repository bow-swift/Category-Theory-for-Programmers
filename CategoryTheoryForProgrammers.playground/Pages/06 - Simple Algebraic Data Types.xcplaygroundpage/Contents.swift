// nef:begin:hidden
import Foundation
// nef:end
/*:
 ```Haskell
 swap :: (a, b) -> (b, a)
 swap (x, y) = (y, x)
 ```
 */
func swap<A, B>(_ t: (A, B)) -> (B, A) {
    let (x, y) = t
    return (y, x)
}
 /*:
 ................
 ```Haskell
 ((a, b), c)
 ```
 ```swift
 ((A, B), C)
 ```
 ................
 ```Haskell
 (a, (b, c))
 ```
 ```swift
 (A, (B, C))
 ```
 ................
 ```Haskell
 alpha :: ((a, b), c) -> (a, (b, c))
 alpha ((x, y), z) = (x, (y, z))
 ```
 */
func alpha<A, B, C>(_ t: ((A, B), C)) -> (A, (B, C)) {
    let ((x, y), z) = t
    return (x, (y, z))
}
 /*:
 ................
 ```Haskell
 alpha_inv :: (a, (b, c)) -> ((a, b), c)
 alpha_inv  (x, (y, z)) = ((x, y), z)
 ```
 */
func alphaInv<A, B, C>(_ t: (A, (B, C))) -> ((A, B), C) {
    let (x, (y, z)) = t
    return ((x, y), z)
}
 /*:
 ................
 ```Haskell
 (a, ())
 ```
 ```swift
 (A, ())
 ```
 ................
 ```Haskell
 rho :: (a, ()) -> a
 rho (x, ()) = x
 ```
 */
func rho<A>(_ t: (A, ())) -> A {
    let (a, _) = t
    return a
}
 /*:
 ................
 ```Haskell
 rho_inv :: a -> (a, ())
 rho_inv x = (x, ())
 ```
 */
func rhoInv<A>(_ x: A) -> (A, ()) {
    (x, ())
}
 /*:
 ................
 ```Haskell
 data Pair a b = P a b
 ```
 */
struct Pair<A, B> {
    let a: A
    let b: B
}
 /*:
 ................
 ```Haskell
 stmt :: Pair String Bool
 stmt = P "This statements is" False
 ```
 */
let stmt: Pair<String, Bool> = Pair(a: "This statement is", b: false)
 /*:
 ................
 ```Haskell
 data Pair a b = Pair a b
 ```
 */
// nef:begin:hidden
class Snippet1 {
// nef:end
typealias Pair<A, B> = (A, B)
 /*:
 ................
 ```Haskell
 stmt = (,) "This statement is" False
 ```
 */
let stmt = ("This statement is", false)
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 data Stmt = Stmt String Bool
 ```
 */
struct Statement {
    let s: String
    let b: Bool
}
 /*:
 ................
 ```Haskell
 startsWithSymbol :: (String, String, Int) -> Bool
 startsWithSymbol (name, symbol, _) = isPrefixOf symbol name
 ```
 */
func startsWithSymbol(_ name: String, _ symbol: String, _: Int) -> Bool {
    name.hasPrefix(symbol)
}
 /*:
 ................
 ```Haskell
 data Element = Element { name         :: String
                        , symbol       :: String
                        , atomicNumber :: Int }
 ```
 */
struct Element {
    let name: String
    let symbol: String
    let atomicNumber: Int
}
 /*:
 ................
 ```Haskell
 tupleToElem :: (String, String, Int) -> Element
 tupleToElem (n, s, a) = Element { name = n
                                 , symbol = s
                                 , atomicNumber = a }
 ```
 */
func tupleToElem(name: String, symbol: String, atomicNumber: Int) -> Element {
    Element(name: name, symbol: symbol, atomicNumber: atomicNumber)
}

// Or alternatively
let tupleToElem2: (String, String, Int) -> Element = Element.init
 /*:
 ................
 ```Haskell
 elemToTuple :: Element -> (String, String, Int)
 elemToTuple e = (name e, symbol e, atomicNumber e)
 ```
 */
func elementToTuple(_ e: Element) -> (String, String, Int) {
    (e.name, e.symbol, e.atomicNumber)
}
 /*:
 ................
 ```Haskell
 atomicNumber :: Element -> Int
 ```
 ```swift
 func atomicNumber(_ e: Element) -> Int
 ```
 ................
 ```Haskell
 startsWithSymbol :: Element -> Bool
 startsWithSymbol e = isPrefixOf (symbol e) (name e)
 ```
 */
func startsWithSymbol(_ e: Element) -> Bool {
    e.name.hasPrefix(e.symbol)
}
 /*:
 ................
 ```Haskell
 startsWithSymbol e = symbol e `isPrefixOf` name e
 ```
 ```swift
let startsWithSymbol : (Element) -> Bool = { e in
    e.name.hasPrefix(e.symbol)
}
 ```
 ................
 ```Haskell
 data Either a b = Left a | Right b
 ```
 */
enum Either<A, B> {
    case left(A)
    case right(B)
}
 /*:
 ................
 ```Haskell
 data OneOfThree a b c = Sinistral a | Medial b | Dextral c
 ```
 */
enum OneOfThree<A, B, C> {
    case sinistral(A)
    case medial(B)
    case dextral(C)
}
 /*:
 ................
 ```Haskell
 Either a Void
 ```
 ```swift
 Either<A, Never>
 ```
 ................
 ```Haskell
 data Color = Red | Green | Blue
 ```
 */
enum Color {
    case red
    case green
    case blue
}
 /*:
 ................
 ```Haskell
 data Bool = True | False
 ```
 */
// nef:begin:hidden
class Snippet2 {
// nef:end
enum Bool {
    case `true`
    case `false`
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 data Maybe a = Nothing | Just a
 ```
 */
enum Option<A> {
    case nothing
    case just(A)
}
 /*:
 ................
 ```Haskell
 data NothingType = Nothing
 ```
 */
typealias NothingType = Never
 /*:
 ................
 ```Haskell
 data JustType a = Just a
 ```
 */
struct JustType<A> {
    let a: A
}
 /*:
 ................
 ```Haskell
 data Maybe a = Either () a
 ```
 ```swift
typealias Option<A> = Either<(), A>
 ```
 ................
 ```Haskell
 List a = Nil | Cons a (List a)
 ```
 */
enum List<A> {
    case `nil`
    indirect case cons(A, List<A>)
}
 /*:
 ................
 ```Haskell
 maybeTail :: List a -> Maybe (List a)
 maybeTail Nil = Nothing
 maybeTail (Cons _ t) = Just t
 ```
 */
func maybeTail<A>(_ list: List<A>) -> Option<List<A>> {
    switch(list) {
    case .nil: return .nothing
    case let .cons(_, t): return .just(t)
    }
}
 /*:
 ................
 ```Haskell
 (a, Either b c)
 ```
 ```swift
 (A, Either<B, C>)
 ```
 ................
 ```Haskell
 Either (a, b) (a, c)
 ```
 ```swift
 Either<(A, B), (A, C)>
 ```
 ................
 ```Haskell
 prodToSum :: (a, Either b c) -> Either (a, b) (a, c)
 prodToSum (x, e) =
     case e of
       Left  y -> Left  (x, y)
       Right z -> Right (x, z)
 ```
 */
func prodToSum<A, B, C>(_ t: (A, Either<B, C>)) -> Either<(A, B), (A, C)> {
    let (x, e) = t
    switch(e) {
    case let .left(y): return .left((x, y))
    case let .right(z): return .right((x, z))
    }
}
 /*:
 ................
 ```Haskell
 sumToProd :: Either (a, b) (a, c) -> (a, Either b c)
 sumToProd e =
     case e of
       Left  (x, y) -> (x, Left  y)
       Right (x, z) -> (x, Right z)
 ```
 */
func sumToProd<A, B, C>(_ e: Either<(A, B), (A, C)>) -> (A, Either<B, C>) {
    switch(e) {
    case let .left(l):
        let (x, y) = l
        return (x, .left(y))
    case let .right(r):
        let (x, z) = r
        return (x, .right(z))
    }
}
 /*:
 ................
 ```Haskell
 prod1 :: (Int, Either String Float)
 prod1 = (2, Left "Hi!")
 ```
 */
let prod1: (Int, Either<String, Float>) = (2, .left("Hi!"))
 /*:
 ................
 ```Haskell
 List a = Nil | Cons a (List a)
 ```
 ```swift
 enum List<A> {
   case `nil`
   case cons(A, List<A>)
 }
 ```
 */

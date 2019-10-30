/*:
 ```Haskell
 absurd :: Void -> a
 ```
 ```swift
 func absurd<A>(_ x: Never) -> A
 ```
 ................
 ```Haskell
 unit :: a -> ()
 unit _ = ()
 ```
 */
func unit<A>(_ a: A) -> () {}
 /*:
 ................
 ```Haskell
 yes :: a -> Bool
 yes _ = True
 ```
 */
func yes<A>(_ a: A) -> Bool {
    true
}
 /*:
 ................
 ```Haskell
 no :: a -> Bool
 no _ = False
 ```
 */
func no<A>(_ a : A) -> Bool {
    false
}
 /*:
 ................
 ```Haskell
 f . g = id
 g . f = id
 ```
 ```swift
 compose(f, g) == id
 compose(g, f) == id
 ```
 ................
 ```Haskell
 fst :: (a, b) -> a
 fst (x, y) = x
 ```
 */
func fst<A, B>(_ x: (A, B)) -> A {
    x.0
}
 /*:
 ................
 ```Haskell
 snd :: (a, b) -> b
 snd (x, y) = y
 ```
 */
func snd<A, B>(_ x: (A, B)) -> B {
    x.1
}
 /*:
 ................
 ```Haskell
 fst (x, _) = x
 snd (_, y) = y
 ```
 ```swift
 func fst<A, B>(_ x: (A, B)) -> A {
     x.0
 }

 func snd<A, B>(_ x: (A, B)) -> A {
     x.1
 }
 ```
 ................
 ```Haskell
 p :: c -> a
 q :: c -> b
 ```
 ```swift
 func p<C, A>(_ x: C) -> A
 func q<C, B>(_ x: C) -> B
 ```
 ................
 ```Haskell
 p :: Int -> Int
 p x = x

 q :: Int -> Bool
 q _ = True
 ```
 */
func p(_ n: Int) -> Int {
    n
}

func q(_ n: Int) -> Bool {
    true
}
 /*:
 ................
 ```Haskell
 p :: (Int, Int, Bool) -> Int
 p (x, _, _) = x

 q :: (Int, Int, Bool) -> Bool
 q (_, _, b) = b
 ```
 */
func p(_ t: (Int, Int, Bool)) -> Int {
    t.0
}

func q(_ t: (Int, Int, Bool)) -> Bool {
    t.2
}
 /*:
 ................
 ```Haskell
 p' = p . m
 q' = q . m
 ```
 ```swift
 p2 == compose(p, m)
 q2 == compose(q, m)
 ```
 ................
 ```Haskell
 m :: Int -> (Int, Bool)
 m x = (x, True)
 ```
 */
func m(_ x: Int) -> (Int, Bool) {
    (x, true)
}
 /*:
 ................
 ```Haskell
 p x = fst (m x) = x
 q x = snd (m x) = True
 ```
 */
// nef:begin:hidden
class Snippet1 {
// nef:end
func p(_ x: Int) -> Int {
    return fst(m(x)) // == x
}

func q(_ x: Int) -> Bool {
    return snd(m(x)) // == true
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 m (x, _, b) = (x, b)
 ```
 */
func m(_ fa: (Int, Int, Bool)) -> (Int, Bool) {
    let (x, _, b) = fa
    return (x, b)
}
 /*:
 ................
 ```Haskell
 fst = p . m'
 snd = q . m'
 ```
 ```swift
 fst == compose(p, m2)
 snd == compose(p, m2)
 ```
 ................
 ```Haskell
 m' (x, b) = (x, x, b)
 ```
 */
func m2(_ fa: (Int, Bool)) -> (Int, Int, Bool) {
    let (x, b) = fa
    return (x, x, b)
}
 /*:
 ................
 ```Haskell
 m' (x, b) = (x, 42, b)
 ```
 */
// nef:begin:hidden
class Snippet2 {
// nef:end
func m2(_ fa: (Int, Bool)) -> (Int, Int, Bool) {
    let (x, b) = fa
    return (x, 42, b)
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 m :: c -> (a, b)
 m x = (p x, q x)
 ```
 */
// nef:begin:hidden
class Snippet3 {
// nef:end
func m(_ x: Int) -> (Int, Bool) {
    (p(x), q(x))
}
// nef:begin:hidden
}
// nef:end
 /*:
 ................
 ```Haskell
 factorizer :: (c -> a) -> (c -> b) -> (c -> (a, b))
 factorizer p q = \x -> (p x, q x)
 ```
 */
func factorizer<A, B, C>(_ p: @escaping (C) -> A, _ q: @escaping (C) -> B) -> (C) -> (A, B) {
    { x in (p(x), q(x)) }
}
 /*:
 ................
 ```Haskell
 i :: a -> c
 j :: b -> c
 ```
 ```swift
 func i<A, C>(_ a : A) -> C
 func j<B, C>(_ b : B) -> C
 ```
 ................
 ```Haskell
 i' = m . i
 j' = m . j
 ```
 ```swift
 i_prime == compose(m, i)
 j_prime == compose(m, j)
 ```
 ................
 ```Haskell
 data Contact = PhoneNum Int | EmailAddr String
 ```
 */
enum Contact {
    case phoneNum(Int)
    case emailAddr(String)
}
 /*
 ................
 ```Haskell
 helpdesk :: Contact;
 helpdesk = PhoneNum 2222222
 ```
 */
let helpdesk: Contact = .phoneNum(2222222)
 /*:
 ................
 ```Haskell
 Either a b = Left a | Right b
 ```
 */
enum Either<A, B> {
    case left(A)
    case right(B)
}
 /*:
 ................
 ```Haskell
 factorizer :: (a -> c) -> (b -> c) -> Either a b -> c
 factorizer i j (Left a)  = i a
 factorizer i j (Right b) = j b
 ```
 */
func factorizer<A, B, C>(_ i: @escaping (A) -> C, _ j : @escaping (B) -> C) -> (Either<A, B>) -> C {
    { x in
        switch(x) {
        case let .left(a): return i(a)
        case let .right(b): return j(b)
        }
    }
}
 /*:
 ................
 ```Haskell
 p = fst . m
 q = snd . m
 ```
 ```swift
 p == compose(fst, m)
 q == compose(snd, m)
 ```
 ................
 ```Haskell
 p () = fst (m ())
 q () = snd (m ())
 ```
 ```swift
 p(()) == fst(m(()))
 q(()) == snd(m(()))
 ```
 */

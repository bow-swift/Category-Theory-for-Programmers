/*:
 ```Haskell
 class Monoid m where
     mempty  :: m
     mappend :: m -> m -> m
 ```
 */
protocol Monoid {
    static func empty() -> Self
    func combine(_ other: Self) -> Self
}
 /*:
 ................
 ```Haskell
 instance Monoid [a] where
     mempty = []
     mappend = (++)
 ```
 */
extension Array: Monoid {
    static func empty() -> [Element] {
        []
    }
    
    func combine(_ other: [Element]) -> [Element] {
        self + other
    }
}
 /*:
 ................
 ```Haskell
 2 * 3 = 6
 [2] ++ [3] = [2, 3] // not the same as [6]
 ```
 */
2 * 3 == 6
[2] + [3] == [2, 3] // not the same as [6]
 /*:
 ................
 ```Haskell
 h (a * b) = h a * h b
 ```
 ```swift
 h(a * b) == h(a) * h(b)
 ```
 ................
 ```Haskell
 [2] ++ [3] = [2, 3]
 ```
 ```swift
 [2] + [3] == [2, 3]
 ```
 ................
 ```Haskell
 2 * 3 = 6
 ```
 ```swift
 2 * 3 == 6
 ```
 ................
 ```Haskell
 p :: x -> U m
 ```
 ```swift
 func p<X, U, M>(_ x: X) -> Kind<U, M>
 ```
 ................
 ```Haskell
 q :: x -> U n
 ```
 ```swift
 func q<X, U, N>(_ x: X) -> Kind<U, N>
 ```
 ................
 ```Haskell
 h :: m -> n
 ```
 ```swift
 func h<M, N>(_ m: M) -> N
 ```
 ................
 ```Haskell
 q = U h . p
 ```
 ```swift
 let q = compose(U.lift(h), p)
 ```
 ................
 ```Haskell
 h a * h e = h (a * e) = h a
 ```
 ```swift
 h(a) * h(e) == h(a * e) == h(a)
 ```
 */

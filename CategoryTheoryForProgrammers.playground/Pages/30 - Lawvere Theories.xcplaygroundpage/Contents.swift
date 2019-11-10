/*:
 ```Haskell
 type Two = Either () ()
 ```
 */
// nef:begin:hidden
import Bow
// nef:end
typealias Two = Either<(), ()>
 /*:
 ................
 ```Haskell
 raise :: () -> a
 ```
 ```swift
 func raise<A>() -> A
 ```
 ................
 ```Haskell
 type Maybe a = Either () a
 ```
 ```swift
 typealias Option<A> = Either<(), A>
 ```
 ................
 ```Haskell
 data Maybe a = Nothing | Just a
 ```
 */
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

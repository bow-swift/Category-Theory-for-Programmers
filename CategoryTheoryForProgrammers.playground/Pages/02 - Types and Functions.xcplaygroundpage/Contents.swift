/*:
 ```Haskell
 x :: Integer
 ```
 ```swift
 let x: Int
 ```
 ................
 ```Haskell
 f :: Bool -> Bool
 ```
 ```swift
 func f(_ x: Bool) -> Bool
 ```
 ................
 ```Haskell
 f :: Bool -> Bool
 f x = undefined
 ```
 ```swift
 func f(_ x: Bool) -> Bool {
     fatalError()
 }
 ```
 ................
 ```Haskell
 f :: Bool -> Bool
 f = undefined
 ```
 ```swift
 let f: (Bool) -> Bool = { _ in fatalError() }
 ```
 ................
 ```Haskell
 fact n = product [1..n]
 ```
 */
func fact(_ n: Int) -> Int {
    (1 ... n).reduce(1, *)
}
 /*:
 ................
 ```Haskell
 absurd :: Void -> a
 ```
 ```swift
 func absurd<A>(_ x: Never) -> A
 ```
 ................
 ```Haskell
 f44 :: () -> Integer
 f44 () = 44
 ```
*/
func f44() -> Int { 44 }
/*:
 ................
 ```Haskell
 fInt :: Integer -> ()
 fInt x = ()
 ```
*/
func fInt(_ x: Int) -> () { }
/*:
 ................
 ```Haskell
 fInt :: Integer -> ()
 fInt _ = ()
 ```
 ```swift
 let fInt: (Int) -> () = { _ in }
 ```
 ................
 ```Haskell
 unit :: a -> ()
 unit _ = ()
 ```
*/
let unit: () -> () = { }
/*:
 ................
 ```Haskell
 data Bool = True | False
 ```
*/
enum Bool {
   case `true`
   case `false`
}

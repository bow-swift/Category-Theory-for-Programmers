/*:
 ```Haskell
 class Monoid m where
     mempty  :: m
     mappend :: m -> m -> m
 ```
*/
protocol Monoid {
   static var mempty: Self { get }
   static func mappend(_ x: Self, _ y: Self) -> Self
}
/*:
 ................
 ```Haskell
 instance Monoid String where
     mempty = ""
     mappend = (++)
 ```
 */
extension String: Monoid {
    static var mempty: String { "" }
    static func mappend(_ x: String, _ y: String) -> String {
        x + y
    }
}
 /*:
 ..............
 */

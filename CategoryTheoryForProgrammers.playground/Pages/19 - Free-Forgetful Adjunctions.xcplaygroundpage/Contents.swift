/*:
 ```Haskell
 type String = [Char]
 ```
 ```swift
 typealias String = [Character]
 ```
 ................
 ```Haskell
 toNat :: [()] -> Int
 toNat = length

 toLst :: Int -> [()]
 toLst n = replicate n ()
 ```
 */
func toNat(_ list: [Void]) -> Int {
    list.count
}

func toLst(_ n: Int) -> [Void] {
    Array(repeating: (), count: n)
}

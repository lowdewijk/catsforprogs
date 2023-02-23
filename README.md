# Category Theory for Programmers Challenges

I will chose Haskell as my favorite programming language. TypeScript will be my second favorite.

## 1. Category: The Essence of Composition

1.  In TypeScript:

```
const id = x => x
```

2.  In Haskell:

```
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)
```

3. In Haskell:

```
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

hello :: String -> String
hello x = x ++ "Hello"

main :: IO ()
main = do
    let eqA = ((compose id hello) "") == hello ""
    let eqB = ((compose hello id ) "") == hello ""
    putStrLn $ show eqA
    putStrLn $ show eqB
```

Will output `TrueTrue`

4. Yes. Pages are objects. Morphisms are links.

5. Yes.

6. When it allows loop edges.



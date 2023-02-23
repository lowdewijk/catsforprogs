compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

hello :: String -> String
hello x = x ++ "Hello"

main :: IO ()
main = do
    let eqA = ((compose id hello) "") == hello ""
    let eqB = ((compose hello id ) "") == hello ""
    putStrLn $ show eqA ++ show eqB

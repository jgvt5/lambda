import Terms

main :: IO ()
main = do
    let x = App (Abst 'y' $ Var 'y') (Var 'x')
    putStrLn $ show x
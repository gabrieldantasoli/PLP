maioresQueTres :: [String] -> [String]
maioresQueTres texto = filter (\palavra -> length palavra > 3) texto

main :: IO()
main = do
    texto <- readLn :: IO String
    print (unwords (maioresQueTres (words texto)))
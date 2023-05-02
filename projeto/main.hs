import System.IO

data Filme = Filme { 
  titulo :: String,
  generos :: [String],
  descricao :: String,
  diretor :: String,
  atores :: [String],
  ano :: String,
  duracao :: String,
  notaUsuarios :: Float,
  usuariosVotates :: String,
  arrecadacao :: String,
  notaImdb :: Int,
  notaUsuario :: Float
} deriving (Show)

parseLine :: String -> Filme
parseLine line = Filme { 
  titulo = fields!!1,
  generos = splitOn "," (fields!!2),
  descricao = fields!!3,
  diretor = fields!!4,
  atores = splitOn "," (fields!!5),
  ano = fields!!6,
  duracao = fields!!7,
  notaUsuarios = read (fields!!8),
  usuariosVotates = fields!!9,
  arrecadacao = fields!!10,
  notaImdb = read (fields!!11),
  notaUsuario = read (fields!!12)
} where
    fields = splitOn "," line

parseCSV :: String -> [Filme]
parseCSV input = map parseLine (tail (lines input))

main = do
  handle <- openFile "filmes.csv" ReadMode
  contents <- hGetContents handle
  let filmes = parseCSV contents
  putStrLn (show filmes)
  hClose handle

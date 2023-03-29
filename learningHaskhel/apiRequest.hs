{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Data.Text (unpack)
import Text.XML.Cursor (Cursor, fromDocument, ($//), ($/), (&|), (&/), (&//), (>=>))
import Text.XML (Document(..), def)
import qualified Data.ByteString.Lazy.Char8 as L8

-- Retorna um cursor que pode ser usado para acessar os elementos do XML
getIMDBCursor :: String -> IO Cursor
getIMDBCursor imdbId = do
  let url = "http://www.omdbapi.com/?i=" ++ imdbId ++ "&plot=xml&apikey=<sua chave de API aqui>"
  response <- simpleHttp url
  let doc = parseLBS def response
  return $ fromDocument doc

-- Obtém o título, ano e classificação de um filme no IMDB com base no ID do IMDB
getMovieInfo :: String -> IO (String, String, String)
getMovieInfo imdbId = do
  cursor <- getIMDBCursor imdbId
  let title = unpack $ cursor $// element "movie" &/ element "title" &| head
      year = unpack $ cursor $// element "movie" &/ element "year" &| head
      rating = unpack $ cursor $// element "movie" &/ element "imdbRating" &| head
  return (title, year, rating)

-- Função principal que chama getMovieInfo e imprime os resultados
main :: IO ()
main = do
  movieInfo <- getMovieInfo "tt1375666" -- Exemplo com o filme "Inception"
  putStrLn $ "Título: " ++ (fst movieInfo)
  putStrLn $ "Ano: " ++ (snd movieInfo)
  putStrLn $ "Classificação: " ++ (trd movieInfo)
  where trd (_, _, x) = x

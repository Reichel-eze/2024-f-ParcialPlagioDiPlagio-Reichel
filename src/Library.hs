module Library where
import PdePreludat

-- AUTORES Y OBRAS --

data Autor = UnAutor {
    nombre :: String,
    obras :: [Obra]
}deriving(Show, Eq)

data Obra = UnaObra {
    texto :: String,
    anio :: Number
}deriving(Show, Eq)

-- 1) Modelar las siguientes obras y que existan autores que las hayan publicado:
-- "Había una vez un pato.", publicada en 1997
-- "¡Habia una vez un pato!", publicada en 1996
-- "Mirtha, Susana y Moria.", publicada en 2010
-- "La semántica funcional del amoblamiento vertebral es riboficiente", publicada en 2020
-- "La semántica funcional de Mirtha, Susana y Moria.", publicada en 2022

habiaUnaVezUnPatoPlagio :: Obra
habiaUnaVezUnPatoPlagio = UnaObra "Había una vez un pato." 1997

habiaUnaVezUnPatoOriginal :: Obra
habiaUnaVezUnPatoOriginal = UnaObra "¡Habia una vez un pato!" 1996

divas :: Obra
divas = UnaObra "Mirtha, Susana y Moria." 2010

semanticaFuncional :: Obra
semanticaFuncional = UnaObra "La semántica funcional del amoblamiento vertebral es riboficiente" 2020

semanticaFuncionalConDivas :: Obra 
semanticaFuncionalConDivas = UnaObra "La semántica funcional de Mirtha, Susana y Moria." 2022

eze :: Autor
eze = UnAutor "Ezequiel" [habiaUnaVezUnPatoPlagio, divas]

marce :: Autor
marce = UnAutor "Marcela" [habiaUnaVezUnPatoOriginal, semanticaFuncional]

-- 2) Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y 
-- quitar signos de puntuación y todo carácter que no sea una letra o un número. 
-- Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"

versionCruda :: String -> String
versionCruda texto = (filter (esLetraONumero) . map transformarAcento) texto

versionCruda' :: String -> String
versionCruda' texto = (map transformarAcento . filter (not . caracterInutil)) texto

transformarAcento :: Char -> Char
transformarAcento 'á'   = 'a'
transformarAcento 'é'   = 'e'
transformarAcento 'í'   = 'i'
transformarAcento 'ó'   = 'o'
transformarAcento 'ú'   = 'u' 
transformarAcento letra = letra 

esLetraONumero :: Char -> Bool
esLetraONumero letra = letra `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ")

caracterInutil :: Char -> Bool
caracterInutil letra = letra `elem` "?¿!¡.,:;-_/()"
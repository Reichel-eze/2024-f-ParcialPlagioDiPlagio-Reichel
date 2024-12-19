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

-- AUTORES --

eze :: Autor
eze = UnAutor "Ezequiel" [habiaUnaVezUnPatoPlagio, divas]

marce :: Autor
marce = UnAutor "Marcela" [habiaUnaVezUnPatoOriginal, semanticaFuncional]

-- 2) Conocer la versión cruda de un texto, que consiste en eliminar los acentos de las letras existentes y 
-- quitar signos de puntuación y todo carácter que no sea una letra o un número. 
-- Por ejemplo, la versión cruda de "Había una vez un pato..." es "Habia una vez un pato"

versionCruda :: String -> String
versionCruda = filter esLetraONumero . map transformarAcento 
-- 1ero. Transformo en texto en un texto sin acentos
-- 2dos. Filtro todo aquello que sea letra o numero (o que no sea un caracter inutil)

versionCruda' :: String -> String
versionCruda' = map transformarAcento . filter (not . caracterInutil)

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

-- Plagios --

-- 3) Se desea detectar si una obra es plagio de la otra. Hay distintas formas de reconocer un plagio, de los cuales se conocen
-- las siguientes, pero podrían haber más. 
-- En cualquier caso, una obra debe haber sido publicada en un año posterior a la obra original para ser considerada un plagio. 

type Plagio = Obra -> Obra -> Bool

--esPlagio :: Obra -> Obra -> Bool
--esPlagio obraPlagio obraOriginal = anio obraPlagio > anio obraOriginal -- && reconocimientoDePlagio obraPlagio obraOriginal

-- En cualquier caso de plagio siempre tiene que pasar esto:

fuePublicadaDespues :: Obra -> Obra -> Bool
fuePublicadaDespues obraPlagio obraOriginal = anio obraPlagio > anio obraOriginal

-- FORMAS DE RECONOCER UN PLAGIO

-- Copia literal: ocurre cuando la versión cruda de una es igual a la de la otra. Por ejemplo, A es plagio de B. 

copiaLiteral :: Plagio
copiaLiteral obraPlagio obraOriginal = versionCruda (texto obraPlagio) == versionCruda (texto obraOriginal) && fuePublicadaDespues obraPlagio obraOriginal

-- Empieza igual: Los primeros caracteres de una obra son los mismos que otra, y su longitud es menor. 
-- La cantidad de caracteres a analizar puede ser variable. Por ejemplo, E es plagio de D para una cantidad 10, 
-- pero no para una cantidad 30.

empiezaIgual :: Number -> Plagio
empiezaIgual cantidad obraPlagio obraOriginal = primerosNCaracteresIguales cantidad obraPlagio obraOriginal && longitudMenor obraPlagio obraOriginal && fuePublicadaDespues obraPlagio obraOriginal

primerosNCaracteresIguales :: Number -> Obra -> Obra -> Bool
primerosNCaracteresIguales n obraPlagio obraOriginal = primerosNCaracteres n obraOriginal == primerosNCaracteres n obraPlagio

primerosNCaracteres :: Number -> Obra -> String
primerosNCaracteres n obra = take n (texto obra) 

longitudMenor :: Obra -> Obra -> Bool
longitudMenor obraPlagio obraOriginal = longitud obraPlagio < longitud obraOriginal 

longitud :: Obra -> Number
longitud = length . texto

-- Le agregaron intro: La obra plagiada empieza a su manera, pero al final incluye totalmente el texto de la original. 
-- Por ejemplo, E es plagio de C.

--leAgregaronIntro :: Plagio
--leAgregaronIntro obraPlagio obraOriginal = nombreIncluidoEnElFinal obraPlagio obraOriginal && fuePublicadaDespues obraPlagio obraOriginal

--nombreIncluidoEnElFinal :: Obra -> Obra -> Bool
--nombreIncluidoEnElFinal obraPlagio obraOriginal = take (( length . versionCruda . texto) obraOriginal) ((reverse . versionCruda . texto) obraPlagio)


-- Inventar otra forma de detectar plagio, utilizando una expresión lambda.

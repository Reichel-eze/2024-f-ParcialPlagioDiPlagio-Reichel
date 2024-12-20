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

leAgregaronIntro :: Plagio
leAgregaronIntro obraPlagio obraOriginal = nombreIncluidoEnElFinal obraPlagio obraOriginal && fuePublicadaDespues obraPlagio obraOriginal

nombreIncluidoEnElFinal :: Obra -> Obra -> Bool
nombreIncluidoEnElFinal obraPlagio obraOriginal = (versionCruda . reverse . texto) obraOriginal == (take ((length . versionCruda . texto) obraOriginal) ((versionCruda . reverse . texto) obraPlagio))

-- Inventar otra forma de detectar plagio, utilizando una expresión lambda.

-- Misma cantidad de vocales 
plagioRaro :: Plagio
plagioRaro = (\obraPlagio obraOriginal -> (length . soloVocales . texto) obraPlagio == (length . soloVocales . texto) obraOriginal)

esVocal :: Char -> Bool 
esVocal letra = letra `elem` "aeiouáéíóúAEIOUÁÉÍÓÚ"

soloVocales :: String -> String
soloVocales = filter (esVocal)

-- BOTS --

-- Existen diferentes bots, y cada uno detecta diversas formas de plagio. Además se conoce su fabricante.
-- 4) Modelar dos bots de ejemplo, incluyendo todas las formas de detección existentes hasta ahora.

data Bot = UnBot {
    formasDePlagio :: [Plagio],
    fabricante :: String
}deriving(Show, Eq)

bot1 :: Bot
bot1 = UnBot [copiaLiteral, empiezaIgual 10] "Sony"

botito :: Bot
botito = UnBot [leAgregaronIntro, plagioRaro] "Nintendo"

-- 5) Un bot detecta si una obra es plagio de otra si verifica alguna de las formas de detección que maneja.

detectarPlagio :: Bot -> Plagio 
detectarPlagio bot obraPlagio obraOriginal = any (\plagio -> plagio obraPlagio obraOriginal) (formasDePlagio bot)

-- 6) Dado un conjunto de autores y un bot, detectar si es una cadena de plagiadores. 
-- Es decir, el segundo plagió al primero, el tercero al segundo, y así. Se considera que un autor plagió a otro cuando
-- alguna de sus obras es plagio de alguna de las del otro según el bot.

cadenaDePlagiadores :: [Autor] -> Bot -> Bool
cadenaDePlagiadores [] _                        = True          
cadenaDePlagiadores [unicoAutor] _              = True
cadenaDePlagiadores [autor1,autor2] bot         = loPlagio autor2 autor1 bot
cadenaDePlagiadores (autor1:autor2:autores) bot = loPlagio autor2 autor1 bot && cadenaDePlagiadores (autor2:autores) bot

loPlagio :: Autor -> Autor -> Bot -> Bool
loPlagio autorPlagiador autorOriginal bot = detectarAlgunPlagio bot (obras autorPlagiador) (obras autorPlagiador) 

detectarAlgunPlagio :: Bot -> [Obra] -> [Obra] -> Bool
detectarAlgunPlagio bot [] _ = False
detectarAlgunPlagio bot _ [] = False
detectarAlgunPlagio bot (obraPlagio1:obrasPlagiadas) (obraOriginal1:obrasOriginales) =
    detectarPlagio bot obraPlagio1 obraOriginal1 || detectarAlgunPlagio bot (obraPlagio1:obrasPlagiadas) obrasOriginales || detectarAlgunPlagio bot obrasPlagiadas (obraOriginal1:obrasOriginales)

-- 7) Dado un conjunto de autores y un bot, encontrar a los autores que  "hicieron plagio pero aprendieron",  
-- que significa que luego de que el bot detectara que una de sus obras fue plagio de alguna de los otros autores, 
-- nunca más volvió a plagiar. En definitiva, su plagio detectado fue el primero y el último.

--hicieronPlagioPeroAprendieron :: Bot -> [Autor] -> [Autor]
--hicieronPlagioPeroAprendieron

-- INFINITO --
-- 8)
-- a) Codificar una obra infinita.

obraInfinita :: Obra
obraInfinita = UnaObra {texto = cycle "Hola ", anio = 2024}

-- b) ¿Qué sucede si se desea verificar si esa obra es plagio de otra con cada una de las formas existentes? 
-- Justificar conceptualmente en cada caso

-- En las diferentes formas existentes de verificar el plagio ocurrira lo siguiente:

-- Si quisiera saber si es una copiaLiteral de otra obra, esto seria imposible de obtener un resultado concreto porque es necesario
-- el conocimiento completo del texto de las obras para verificar si son coincidentes en sus versiones crudas

-- En el caso de leAgregaronIntro ocurre lo mismo con respecto al resultado que en copiaLiterla, aca se necesita tener 
-- conocimiento del final de la obra plagio y como justamente la obra que es plagio es la obra infinita, entonces nunca
-- se llegara al final de la misma porque literalmente no tiene un final determinado entonces ni puedo decir si en dicho
-- final se encuentro el texto de la obra original

-- Para la situacion de empiezaIgual si se podria llegar a un resultado debido a que en este caso no es necesario tener un
-- conocimiento de todo el texto de la obra que realizo el plagio, simplemente agarro una n cantidad de caracteres y los
-- comparo con la misma n cantidad de caracteres de la obra original y veo si son iguales. Esto es perfectamente posible gracias
-- a la evaluacion perezosa (lazy evaluation) que utiliza haskell, primero siempre evalua lo que tiene que hacer y luego que
-- parametros utilizar. Asi que por ejemplo, si solo necesita las primeras 10 letras del texto de la obra infinita, solo 
-- "calculara" la obra hasta obtener las 10 letras, sin ser necesario calcular lo que resta del texto de la obra (NO va a 
-- trabajar sobre los parametros/o su totalidad a menos que sea estrictamente necesario)

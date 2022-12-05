-- Alexander
-- 29.12.2022
-- Uebung Einheit 6
module UE6 where
import Data.Char

---------------------------------------------------------------------------------------------------------------------------------------------------
-- Aufgabe_1: a

-- I) Gib einen Ausdruck an, indem alle fünf Arten auftauchen. (Konstanten, arithmetische, boolesche und bedingte Ausdrücke sowie Funktionsaufrufe beinhalten)
-- II) Typ des Ausdrucks soll String sein.In der Lage den Wert des Ausdrucks zu berechnen.
-- III) Welchen Wert hat euer Ausdruck und wie kommt dieser zustande? Begründe außerdem, warum dein Ausdruck alle genannten Forderungen erfüllt.

a,c::Integer
a = 3+2
c = 0

-- Spezifiakation:  Vorraussetzung: zwei Eingaben: string und ein bool Wert. / Ergebnis: Eine Addition

myFunction :: String -> Bool -> Integer
myFunction word b = if (b == True) then a else c

-- Welchen Wert hat euer Ausdruck: String und ein Wahrheitswert. Durch die Eingabe des Strings und den entpsrechenden Wahrheitswert wird mit der
                                   -- If-Anweisung der Eingegebene Wahrheitswert Überprüft und entweder a ausgegeben oder c. Wenn a, dann wird eine Arithmetische Operation durchgeführt.
-- Wie kommt es zustande ? Durch die Eingabe des Users.


-- Ausdruck erfüllt alle Anforderungen. Es Enthält Konstanten wie True, ein arithmetischen Wert wie a = 3+2, ein booleschen Wert wie b=Bool,
                      -- eine bedingten Ausdruck durch die If-Anweisung und ein Funktionsaufruf durch die Deklaration: myFunction.
---------------------------------------------------------------------------------------------------------------------------------------------------

-- Aufgabe_1: b

-- I) Implementiere eine Funktion calc

-- Spezifikatin: Voraussetzung: drei Eingabe ! zwei Zahlen und ein String / Ergebnis: Multiplikation, Addition, Subtraktion, Quadratition
calc :: Integer -> String -> Integer -> Integer
calc a op b
   | op == "plus" = a+b
   | op == "minus" = a-b
   | op == "mal" = a*b
   | op == "hoch" && b >= 0 = a^b
   | otherwise = error"Fehler"

-- Aufgabe_1: c

-- I) Implementiere eine Funktion spanne

-- Bsp: spanne (-2) 0 42  3 = 44)




spanne :: Int -> Int -> Int -> Int -> Int
spanne a b c d
    | a <= b && b <= c && b >= c && c <= d = d-a
    | a <= b && b <= c && c >= d = c-a
    | a >= b && b <= c && c <= d = d-b
    | a >= b && b <= c && c >= d = c-b
    | a <= b && a >= b && b >= c && c <= d = d-c
    | a <= b && a >= b && b <= c && c >= d = c-d
    | a <= b && b >= c && c >= d = b-a
    | a <= b && b >= c && c <= d = b-c
    | a <= b && b >= c && c >= d = b-d
    | a >= b && b <= c && c <= d && c >= d = a-b
    | a >= b && b >= c && c <= d = a+c
    | a >= b && b <= c && b >= c && c >= d = a-d

-- Augabe_1: d

-- I) Implementiere eine Funktion spiegeln
spiegeln :: Char -> Char
spiegeln n
    | 'A' == n = chr( ord n + 25)
    | 'B' == n = chr( ord n + 24-1)
    | 'C' == n = chr( ord n + 23-2)
    | 'D' == n = chr( ord n + 22-3)
    | 'E' == n = chr( ord n + 21-4)
    | 'F' == n = chr( ord n + 20-5)
    | 'G' == n = chr( ord n + 19-6)
    | 'H' == n = chr( ord n + 18-7)
    | 'I' == n = chr( ord n + 17-8)
    | 'J' == n = chr( ord n + 16-9)
    | 'K' == n = chr( ord n + 15-10)
    | 'L' == n = chr( ord n + 14-11)
    | 'M' == n = chr( ord n + 13-12)
    | 'N' == n = chr( ord n + 12-13)
    | 'O' == n = chr( ord n + 11-14)
    | 'P' == n = chr( ord n + 10-15)
    | 'Q' == n = chr( ord n + 9-16)
    | 'R' == n = chr( ord n + 8-17)
    | 'S' == n = chr( ord n + 7-18)
    | 'T' == n = chr( ord n + 6-19)
    | 'U' == n = chr( ord n + 5-20)
    | 'V' == n = chr( ord n + 4-21)
    | 'W' == n = chr( ord n + 3-22)
    | 'X' == n = chr( ord n + 2-23)
    | 'Y' == n = chr( ord n + 1-24)
    | 'Z' == n = chr( ord n + 0-25)
    | otherwise = error"Bitte nur Großbuchstaben eingeben !"



-- Aufgabe_2: a

-- Input: natürliche Zahlen n
-- Output: Anzahl der Nullen in der Binärdarstellung von n (Bsp: 18 → 3, denn 18 = 10010)

-- Spezifikatin: Voraussetzung: n>=0 / Ergebnis: Die addition der Nullen einer Dualzahl.

deztoBin :: Int -> Int
deztoBin 0 = 1 -- Rekursionsanker
deztoBin 1 = 0 -- Rekursionsanker
deztoBin n = if (n `mod` 2 == 0) then deztoBin(n `div` 2)+1 else deztoBin(n `div` 2)


















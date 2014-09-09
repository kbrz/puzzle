Magnesy - Metody Programowania 2013
======

Łamigłówka „Magnesy” z numeru 12/2000.

Dane wejściowe: rozmiar planszy m × n (m, n ∈ {1,...,16}), lista list wartości typu Field
```
data Field = U | R | D | L | UN | RN | DN | LN | US | RS | DS | LS
```

oznaczających podział planszy (reprezentowanej wierszami) na magnesy oraz listy biegunów północnych w kolumnach i wierszach oraz południowych w kolumnach i wierszach. Wartość U oznacza, że drugi biegun magnesu znajduje się w polu powyżej, R — na prawo, D — poniżej, zaś L — na lewo. Wartości z literą N na końcu reprezentują położenie ujawnionych biegunów północnych. Wśród danych wejściowych nie występują bieguny południowe (tzn. wartości US, RS, DS i LS).

Dane wyjściowe: lista list wartości typu Field, reprezentująca (wierszami) planszę stanowiącą rozwiązanie (wartości U, R, D, L reprezentują połówki nienamagnesowanych kostek, UN, RN, DN, LN — bieguny północne, zaś US, RS, DS, LS — południowe).

Należy napisać program zawierający funkcję:
```
solve :: Int -> Int -> [[Field]] -> [Int] -> [Int] -> [Int] -> [Int] -> [Result]
```

gdzie typ Result jest zdefiniowany następująco:
```
type Result = [[Field]]
```

Przykładowe wywołanie jest następujące:
```
solve 6 6 [[D,D,R,L,R,L], [U,U,D,D,R,L], [R,L,U,U,D,D], [D,RN,L,D,U,U], [U,R,L,U,R,L], [R,L,R,L,R,L]] [3,3,3,3,3,3] [3,3,3,3,3,3] [3,3,3,3,3,3] [3,3,3,3,3,3]
```

Powinno ono zwrócić wartość:
```
[[[DN,DS,RN,LS,RN,LS], 
  [US,UN,DS,DN,RS,LN], 
  [RN,LS,UN,US,DN,DS], 
  [DS,RN,LS,DN,US,UN], 
  [UN,RS,LN,US,RN,LS], 
  [RS,LN,RS,LN,RS,LN]]] 
```

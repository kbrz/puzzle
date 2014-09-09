import Puzzle
import Checker


-- Funkcja zawN zwraca ilosc wystapien biegunow N w wierszu
--
-- Argumenty przyjmowane przez funkcje:
--  wiersz, w ktorym maja zostac zliczone bieguny - :t [Field]
--
-- Wartosc zwracana:
--  ilosc wystapien biegunow N w wierszu - :t Int
zawN :: [Field] -> Int
zawN [] = 0
zawN (x:xs)
   | elem x [DN,UN,RN,LN]   = 1 + zawN xs
   | otherwise              = zawN xs


-- Funckja zawS zwraca ilosc wystapien biegunow S w wierszu
--
-- Argumenty przyjmowane przez funkcje:
--  wiersz, w ktorym maja zostac zliczone bieguny - :t [Field]
--
-- Wartosc zwracana:
--  ilosc wystapien biegunow S w wierszu - :t Int
zawS :: [Field] -> Int
zawS [] = 0
zawS (x:xs)
   | elem x [DS,US,RS,LS]   = 1 + zawS xs
   | otherwise              = zawS xs


-- Funckja listaPoprawnychWierszy generuje liste wszystkich poprawnych unifikacji, ktore moga powstac z wiersza podanego jako argument
-- przy warunkach podanych jako argumenty. Funkcja jest rekurencyjna i dziala w oparciu o mechanizm list comprehension. Wyodrebnionych
-- jest 5 przypadkow, z czego 4 podzielone sa na 2 podprzypadki (dla braku poprzedniego wiersza (generowanie unifikacji pierwszego
-- wiersza w planszy) oraz istniejacego poprzedniego wiersza).
--
-- Argumenty przyjmowane przez funkcje:
--  liczba wystapien biegunow N w wierszu - :t Int
--  liczba wystapien biegunow S w wierszu - :t Int
--  lista pozostalych wystapien biegunow N w kolejnych kolumnach planszy - :t [Int]
--  lista pozostalych wystapien biegunow S w kolejnych kolumnach planszy - :t [Int]
--  poprzedni wiersz planszy - :t [Field]
--  wiersz, z ktorego maja powstawac poprawnie zunifikowane wiersze - :t [Field]
--
-- Wartosc zwracana:
--  lista wszystkich poprawnych unifikacji wiersza przekazanego w argumencie - :t [[Field]]
listaPoprawnychWierszy :: Int -> Int -> [Int] -> [Int] -> [Field] -> [Field] -> [[Field]]

-- Przypadek 1 (bazowy).
--  Jedyna mozliwa unifikacja pustego wiersza jest wiersz pusty.
listaPoprawnychWierszy 0 0 [] [] [] [] = [[]]

-- Przypadek 2.
--  Unifikacja wiersza jednoelementowego zgodnie z zasadami zadania. Wiadomo, ze jezeli wiersz ma jeden element, to drugi koniec klocka
--  musi znajdowac sie w wierszu powyzej lub ponizej. W przypadku unifikacji wiersza, ktory nie jest pierwszym wierszem planszy dodane
--  sa warunki zabraniajace wystapien biegunow jednoimiennych jeden pod drugim.
listaPoprawnychWierszy nw sw [nk] [sk] [] [x]
   | x == DN             = [[DN] | nw==1, sw==0, nk>0]
   | x == DS             = [[DS] | sw==1, nw==0, sk>0]
   | x == D              = [x | x <- [[D],[DS],[DN]], zawN x == nw, zawN x <= nk, zawS x == sw, zawS x <= sk]
   | x == UN             = [[UN] | nw==1, sw==0, nk>0]
   | x == US             = [[US] | sw==1, nw==0, sk>0]
   | x == U              = [x | x <- [[U],[US],[UN]], zawN x == nw, zawN x <= nk, zawS x == sw, zawS x <= sk]

listaPoprawnychWierszy nw sw [nk] [sk] [p] [x]
   | x == DN             = [[DN] | nw==1, sw==0, nk>0, notElem p [DN,UN,RN,LN]]
   | x == DS             = [[DS] | sw==1, nw==0, sk>0, notElem p [DS,US,RS,LS]]
   | x == D              = [[D] | nw==0, sw==0] 
                           ++ [[DN] | nw==1, sw==0, nk>0, notElem p [DN,UN,RN,LN]] 
                           ++ [[DS] | nw==0, sw==1, sk>0, notElem p [DS,US,RS,LS]]
   | x == UN             = [[UN] | nw==1, sw==0, nk>0, p==DS]
   | x == US             = [[US] | sw==1, nw==0, sk>0, p==DN]
   | x == U              = [[U] | nw==0, sw==0, p==D] 
                           ++ [[UN] | nw==1, sw==0, nk>0, p==DS] 
                           ++ [[US] | nw==0, sw==1, sk>0, p==DN]

-- Przypadek 3.
--  Glowa unifikowanego wiersza jest kolecek, ktorego druga czesc jest w wierszu ponizej lub powyzej. Wynikiem jest lista wierszy, ktore
--  spelniaja warunki zadane w argumentach, nie posiadaja dwoch biegunow jednoimiennych obok siebie i jesli wiersz nie jest pierwszym
--  wierszem planszy, to wygenerowane wiersze pasuja do wiersza poprzedniego. Glowa poprawnie zunifikowanego wiersza jest klocek spelniajacy
--  zadane warunki, a ogonem zunifikowana reszta wiersza spelniajaca odpowiednio zmodyfikowane warunki.
listaPoprawnychWierszy nw sw (nk:rnk) (sk:rsk) [] (x:xs)
   | x == DN             = [(DN:nxt:rs) | nk>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk [] xs), notElem nxt [DN,UN,RN]]
   | x == DS             = [(DS:nxt:rs) | sk>0, (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]]
   | x == D              = [(D:rs) | rs <- (listaPoprawnychWierszy nw sw rnk rsk [] xs)] 
                           ++ [(DN:nxt:rs) | nk>0, nw>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk [] xs), notElem nxt [DN,UN,RN]] 
                           ++ [(DS:nxt:rs) | sk>0, sw>0, (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]]
   | x == UN             = [(UN:nxt:rs) | nk>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk [] xs), notElem nxt [DN,UN,RN]]
   | x == US             = [(US:nxt:rs) | sk>0, (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]]
   | x == U              = [(U:rs) | rs <- (listaPoprawnychWierszy nw sw rnk rsk [] xs)] 
                           ++ [(UN:nxt:rs) | nk>0, nw>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk [] xs), notElem nxt [DN,UN,RN]] 
                           ++ [(US:nxt:rs) | sk>0, sw>0, (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]]

listaPoprawnychWierszy nw sw (nk:rnk) (sk:rsk) (p:ps) (x:xs)
   | x == DN             = [(DN:nxt:rs) | nk>0, notElem p [DN,UN,RN,LN], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk ps xs), notElem nxt [DN,UN,RN]]
   | x == DS             = [(DS:nxt:rs) | sk>0, notElem p [DS,US,RS,LS], (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]]
   | x == D              = [(D:rs) | rs <- (listaPoprawnychWierszy nw sw rnk rsk ps xs)] 
                           ++ [(DN:nxt:rs) | nk>0, nw>0, notElem p [DN,UN,RN,LN], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk ps xs), notElem nxt [DN,UN,RN]] 
                           ++ [(DS:nxt:rs) | sk>0, sw>0, notElem p [DS,US,RS,LS], (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]]
   | x == UN             = [(UN:nxt:rs) | nk>0, p==DS, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk ps xs), notElem nxt [DN,UN,RN]]
   | x == US             = [(US:nxt:rs) | sk>0, p==DN, (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]]
   | x == U              = [(U:rs) | p==D, rs <- (listaPoprawnychWierszy nw sw rnk rsk ps xs)] 
                           ++ [(UN:nxt:rs) | nk>0, nw>0, p==DS, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) sw rnk rsk ps xs), notElem nxt [DN,UN,RN]] 
                           ++ [(US:nxt:rs) | sk>0, sw>0, p==DN, (nxt:rs) <- (listaPoprawnychWierszy nw (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]]

-- Przypadek 4.
--  Unifikacja wiersza dwuelementowego zgodnie z zasadami zadania. Jezeli argumenty nie wpasowaly sie do poprzednich definicji funkcji, to znaczy
--  ze wiersz sklada sie z jednego klocka ulozonego poziomo. W przypadku unifikacji wiersza, ktory nie jest pierwszym wierszem planszy dodane
--  sa warunki zabraniajace wystapien biegunow jednoimiennych jeden pod drugim.
listaPoprawnychWierszy nw sw [nk1,nk2] [sk1,sk2] [] [x,y]
   | x == R && y == L    = [[e1,e2] | [e1,e2] <- [[R,L],[RS,LN],[RN,LS]], zawN [e1,e2] == nw, zawN [e1] <= nk1, zawN [e2] <= nk2, zawS [e1,e2] == sw, zawS [e1] <= sk1, zawS [e2] <= sk2]
   | x == RN             = [[RN,LS] | nw==1, sw==1, nk1>0, sk2>0]
   | x == RS             = [[RS,LN] | nw==1, sw==1, sk1>0, nk2>0]
   | y == LN             = [[RS,LN] | nw==1, sw==1, sk1>0, nk2>0]
   | y == LS             = [[RN,LS] | nw==1, sw==1, nk1>0, sk2>0]

listaPoprawnychWierszy nw sw [nk1,nk2] [sk1,sk2] [p1,p2] [x,y]
   | x == R && y == L    = [[R,L] | nw==0, sw==0] 
                           ++ [[RN,LS] | nw==1, sw==1, nk1>0, sk2>0, notElem p1 [DN,UN,RN,LN], notElem p2 [DS,US,RS,LS]] 
                           ++ [[RS,LN] | nw==1, sw==1, sk1>0, nk2>0, notElem p1 [DS,US,RS,LS], notElem p2 [DN,UN,RN,LN]]
   | x == RN             = [[RN,LS] | nw==1, sw==1, nk1>0, sk2>0, notElem p1 [DN,UN,RN,LN], notElem p2 [DS,US,RS,LS]]
   | x == RS             = [[RS,LN] | nw==1, sw==1, sk1>0, nk2>0, notElem p1 [DS,US,RS,LS], notElem p2 [DN,UN,RN,LN]]
   | y == LN             = [[RS,LN] | nw==1, sw==1, sk1>0, nk2>0, notElem p1 [DS,US,RS,LS], notElem p2 [DN,UN,RN,LN]]
   | y == LS             = [[RN,LS] | nw==1, sw==1, nk1>0, sk2>0, notElem p1 [DN,UN,RN,LN], notElem p2 [DS,US,RS,LS]]

-- Przypadek 5.
--  Jezeli agrumenty nie pasowaly do poprzednich definicj funkcji, to znaczy ze wiersz ma na poczatku klocek poziomy. Zasada dzialania funkcji
--  analogiczna do przypadku 3.
listaPoprawnychWierszy nw sw (nk1:nk2:rnk) (sk1:sk2:rsk) [] (x1:x2:xs)
   | x1 == RN            = [(RN:LS:nxt:rs) | nk1>0, sk2>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]]
   | x1 == RS            = [(RS:LN:nxt:rs) | sk1>0, nk2>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk [] xs), notElem nxt [DN,UN,RN]]
   | x2 == LN            = [(RS:LN:nxt:rs) | sk1>0, nk2>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk [] xs), notElem nxt [DN,UN,RN]]
   | x2 == LS            = [(RN:LS:nxt:rs) | nk1>0, sk2>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]]
   | x1 == R && x2 == L  = [(R:L:rs) | rs <- (listaPoprawnychWierszy nw sw rnk rsk [] xs)]
                           ++ [(RN:LS:nxt:rs) | nk1>0, sk2>0, nw>0, sw>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk [] xs), notElem nxt [DS,US,RS]] 
                           ++ [(RS:LN:nxt:rs) | sk1>0, nk2>0, sw>0, nw>0, (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk [] xs), notElem nxt [DN,UN,RN]]

listaPoprawnychWierszy nw sw (nk1:nk2:rnk) (sk1:sk2:rsk) (p1:p2:ps) (x1:x2:xs)
   | x1 == RN            = [(RN:LS:nxt:rs) | nw>0, sw>0, nk1>0, sk2>0, notElem p1 [DN,UN,RN,LN], notElem p2 [DS,US,RS,LS], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]]
   | x1 == RS            = [(RS:LN:nxt:rs) | nw>0, sw>0, sk1>0, nk2>0, notElem p1 [DS,US,RS,LS], notElem p2 [DN,UN,RN,LN], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk ps xs), notElem nxt [DN,UN,RN]]
   | x2 == LN            = [(RS:LN:nxt:rs) | nw>0, sw>0, sk1>0, nk2>0, notElem p1 [DS,US,RS,LS], notElem p2 [DN,UN,RN,LN], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk ps xs), notElem nxt [DN,UN,RN]]
   | x2 == LS            = [(RN:LS:nxt:rs) | nw>0, sw>0, nk1>0, sk2>0, notElem p1 [DN,UN,RN,LN], notElem p2 [DS,US,RS,LS], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]]
   | x1 == R && x2 == L  = [(R:L:rs) | rs <- (listaPoprawnychWierszy nw sw rnk rsk ps xs)]
                           ++ [(RN:LS:nxt:rs) | nk1>0, sk2>0, nw>0, sw>0, notElem p1 [DN,UN,RN,LN], notElem p2 [DS,US,RS,LS], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk ps xs), notElem nxt [DS,US,RS]] 
                           ++ [(RS:LN:nxt:rs) | sk1>0, nk2>0, sw>0, nw>0, notElem p1 [DS,US,RS,LS], notElem p2 [DN,UN,RN,LN], (nxt:rs) <- (listaPoprawnychWierszy (nw-1) (sw-1) rnk rsk ps xs), notElem nxt [DN,UN,RN]]


-- Funkcja noweListyNSKolumn zwraca nowe listy ilosci wystapien biegunow N i S w kolumnach. Tzn. jezeli na pozycji i w liscie ilosci wystapien
-- biegunow N w kolumnach jest wartosc n, a w liscie ilosci wystapien biegunow S w kolumnach jest wartosc s, to w nowych listach na tej pozycji
-- beda odpowiednio wartosci n i s jezeli na pozycji i w wierszu jest biegun obojetny, n-1 i s jezeli jest biegun N lub n i s-1 jezeli jest biegun S
--
-- Argumenty przyjmowane przez funkcje:
--  lista ilosci wystapien biegunow N w kolumnach - :t [Int]
--  lista ilosci wystapien biegunow S w kolumnach - :t [Int]
--  wiersz przez ktory maja zostac zmodyfikowane listy - :t [Field]
--
-- Wartosc zwracana przez funkcje:
--  para, ktora na pierwszej pozycji posiada nowa liste ilosci pozostalych wystapien bieguna N w kolumnach, a na drugiej liste ilosci pozostalych
--  wystapien bieguna S w kolumnach - :t ([Int], [Int])
noweListyNSKolumn :: [Int] -> [Int] -> [Field] -> ([Int], [Int])
noweListyNSKolumn _ _ [] = ([], [])
noweListyNSKolumn (n:rListaNKolumna) (s:rListaSKolumna) (w:rWiersz)
   | elem w [D, U, R, L]                = ((n:ns), (s:ss))
   | elem w [DN, UN, RN, LN]            = (((n-1):ns), (s:ss))
   | elem w [DS, US, RS, LS]            = ((n:ns), ((s-1):ss)) where
      (ns, ss) = noweListyNSKolumn rListaNKolumna rListaSKolumna rWiersz


-- Funkcja sprawdz poprawnosc kolumn informuje czy plansza zostala poprawnie zunifikowana pod wzgledem ilosci biegunow w kolumnach lub w trakcie 
-- unifikacji o tym czy jest sens dalszych obliczen. Jezeli plansza zostala juz cala zunifikowana, to ilosc pozostalych wystapien biegunow N i S 
-- we wszystkich kolumnach musi byc rowna 0. Jezeli jestesmy w trakcie unifikacji, to ilosc pozostalych wystapien biegunow N i S we wszystkich 
-- kolumnach musi byc wieksza lub rowna 0.
--
-- Argumenty przyjmowane przez funkcje:
--  dlugosc wiersza - :t Int
--  plansza zunifikowanych wierszy - :t [[Field]]
--  lista pozostalej ilosci wystapien biegunow N w kolumnach - :t [Int]
--  lista pozostalej ilosci wystapien biegunow S w kolumnach - :t [Int]
--
-- Wartosc zwracana przez funkcje:
--  True jezeli argumenty spelniaja warunki podane w opisie lub False w p. p.
sprawdzPoprawnoscKolumn :: Int -> [[Field]] -> [Int] -> [Int] -> Bool
sprawdzPoprawnoscKolumn n resztaWyniku listaNKolumny listaSKolumny
   | resztaWyniku == []   = (listaNKolumny == [0 | x <- [1..n]]) && (listaSKolumny == [0 | x <- [1..n]])
   | otherwise            = aux listaNKolumny && aux listaSKolumny where
      aux [] = True
      aux (e:es)
         | e<0      = False
         |otherwise = aux es


-- Funkcja solve znajduje wszystkie rozwiazania zagadki podanej jako agrumenty (zgodnie z opisem zadania). Metoda znajdowania rozwiazan jest
-- nastepujaca: jedynym rozwiazanie zadania pustego jest lista pusta. W przeciwnym przypadku tworzona jest lista rozwiazan w postaci plansz,
-- ktore w glowie maja poprawnie zunifikowany wiersz, a w ogonie poprawnie zunifikowana plansze pomniejszona o ten jeden wiersz.
solve :: Int -> Int -> [[Field]] -> [Int] -> [Int] -> [Int] -> [Int] -> [Result]
solve  = aux [] where
   aux _ 0 _ _ _ _ _ _  = [[]]
   aux poprzedniWiersz m n (w:wiersze) listaNKolumny (ln:listaNWiersze) listaSKolumny (ls:listaSWiersze) =
      [poprawnyWiersz:resztaWyniku | poprawnyWiersz <- (listaPoprawnychWierszy ln ls listaNKolumny listaSKolumny poprzedniWiersz w)
      , let (listaNKolumny2, listaSKolumny2) = noweListyNSKolumn listaNKolumny listaSKolumny poprawnyWiersz
      , resztaWyniku <- aux poprawnyWiersz (m-1) n wiersze listaNKolumny2 listaNWiersze listaSKolumny2 listaSWiersze
      , sprawdzPoprawnoscKolumn n resztaWyniku listaNKolumny2 listaSKolumny2 == True]


-- Testy do zadania
--  najdluzszy test (nr 4) wykonuje sie 73.51 s.
tests :: [Test]
tests = [
   SimpleTest 
   (Puzzle 4 4 
   [[D,D,D,D], 
   [U,U,U,U], 
   [D,D,D,D], 
   [U,U,U,U]] 
   [1,1,0,0] 
   [1,0,1,0] 
   [1,1,0,0] 
   [0,1,0,1]) 
   [[D,DN,D,D], 
   [U,US,U,U], 
   [DN,D,D,D], 
   [US,U,U,U]],
   
   SimpleTest
   (Puzzle 8 8
   [[R,L,R,L,D,D,R,L],
   [D,R,L,D,U,U,R,L],
   [U,D,D,U,R,L,R,L],
   [D,U,U,R,L,R,L,D],
   [U,R,L,D,DS,D,D,U],
   [R,L,D,U,UN,U,U,D],
   [D,D,U,R,L,R,L,U],
   [U,U,R,L,R,L,R,L]]
   [3,2,3,1,3,2,3,4]
   [4,1,2,3,2,2,4,3]
   [2,3,3,2,2,1,4,4]
   [3,2,2,3,3,3,2,3])
   [[RN,LS,RN,LS,DN,D,RS,LN],
   [D,R,L,D,US,U,RN,LS],
   [U,DN,DS,U,R,L,RS,LN],
   [DN,US,UN,RS,LN,R,L,DS],
   [US,R,L,D,DS,DN,DS,UN],
   [R,L,DS,U,UN,US,UN,DS],
   [DN,DS,UN,R,L,RN,LS,UN],
   [US,UN,RS,LN,R,L,RN,LS]],
   
   SimpleTest
   (Puzzle 10 8
   [[R,L,R,L,R,L,R,L],
   [D,D,D,D,D,D,D,D],
   [U,U,U,U,U,U,U,U],
   [R,L,R,L,R,L,R,L],
   [D,D,D,D,D,D,D,D],
   [U,U,U,U,U,U,U,U],
   [R,L,R,L,R,L,R,L],
   [D,D,D,D,D,D,D,D],
   [U,U,U,U,U,U,U,U],
   [R,L,R,L,R,L,R,L]]
   [3,3,0,3,3,0,3,3]
   [0,3,3,0,3,3,0,3,3,0]
   [3,3,0,3,3,0,3,3]
   [0,3,3,0,3,3,0,3,3,0])
   [[R,L,R,L,R,L,R,L],
   [DN,DS,D,DN,DS,D,DN,DS],
   [US,UN,U,US,UN,U,US,UN],
   [R,L,R,L,R,L,R,L],
   [DN,DS,D,DN,DS,D,DN,DS],
   [US,UN,U,US,UN,U,US,UN],
   [R,L,R,L,R,L,R,L],
   [DN,DS,D,DN,DS,D,DN,DS],
   [US,UN,U,US,UN,U,US,UN],
   [R,L,R,L,R,L,R,L]],
   
   SimpleTest
   (Puzzle 15 14
   [[DN,R,L,DS,R,L,D,R,L,DS,R,L,R,LS],
   [U,R,L,UN,R,L,U,R,L,U,R,L,R,L],
   [DN,D,R,L,DN,D,R,L,D,D,D,R,L,DS],
   [U,U,R,L,U,U,D,D,U,U,U,D,D,U],
   [R,L,RN,L,R,L,U,U,R,LS,D,US,U,DS],
   [D,RN,L,D,D,R,L,R,L,D,U,R,L,U],
   [U,RS,L,U,U,D,R,L,D,U,R,LS,D,D],
   [RS,L,RS,L,D,U,R,L,U,R,L,D,U,U],
   [R,L,R,L,U,D,D,R,L,DS,D,U,R,L],
   [D,RN,L,R,L,U,US,R,L,U,U,R,L,D],
   [U,DS,R,L,R,L,R,L,D,D,R,L,D,US],
   [DS,UN,R,L,R,L,D,D,U,U,R,L,U,D],
   [U,D,DN,R,L,D,U,U,D,R,L,D,D,U],
   [DS,U,U,D,D,U,R,L,U,D,D,U,U,D],
   [U,R,L,U,U,R,L,R,L,U,U,R,L,U]]
   [5,7,3,5,5,5,6,4,5,4,8,4,5,5]
   [4,5,5,5,4,5,4,6,6,4,3,5,6,4,5]
   [5,5,6,4,4,6,4,6,4,5,6,7,3,6]
   [5,4,5,5,4,5,4,6,5,4,5,3,6,5,5])
   [[DN,R,L,DS,RN,LS,D,R,L,DS,RN,LS,RN,LS],
   [US,RN,LS,UN,R,L,U,RN,LS,UN,R,L,RS,LN],
   [DN,DS,R,L,DN,DS,RN,LS,D,D,DN,RS,LN,DS],
   [US,UN,RS,LN,US,UN,DS,D,U,U,US,DN,D,UN],
   [R,L,RN,LS,R,L,UN,U,RN,LS,DN,US,U,DS],
   [D,RN,LS,D,D,RN,LS,RN,LS,D,US,RN,LS,UN],
   [U,RS,LN,U,U,DS,RN,LS,DN,U,RN,LS,D,D],
   [RS,LN,RS,LN,DS,UN,RS,LN,US,RN,LS,DN,U,U],
   [RN,LS,R,L,UN,D,DN,RS,LN,DS,DN,US,RN,LS],
   [D,RN,LS,RN,LS,U,US,R,L,UN,US,R,L,DN],
   [U,DS,R,L,RN,LS,RN,LS,D,D,RN,LS,D,US],
   [DS,UN,R,L,RS,LN,D,DN,U,U,RS,LN,U,DN],
   [UN,DS,DN,RS,LN,DS,U,US,DN,RS,LN,D,DN,US],
   [DS,UN,US,DN,D,UN,R,L,US,DN,DS,U,US,D],
   [UN,R,L,US,U,RS,LN,RS,LN,US,UN,RS,LN,U]],
   
   CountTest
   (Puzzle 1 2
   [[R,L]]
   [1,0]
   [1]
   [0,1]
   [1])
   1,
   
   CountTest
   (Puzzle 2 2
   [[R,L],
   [R,L]]
   [1,1]
   [1,1]
   [1,1]
   [1,1])
   2,
   
   CountTest
   (Puzzle 1 6
   [[R,L,R,L,R,L]]
   [1,1,0,0,0,0]
   [1]
   [0,1,0,0,0,0]
   [1])
   0,
   
   CountTest
   (Puzzle 1 6
   [[R,L,R,L,R,L]]
   [1,1,0,0,0,0]
   [2]
   [0,0,0,0,0,0]
   [1])
   0,
   
   CountTest
   (Puzzle 7 4
   [[R,L,D,D],
   [R,L,U,U],
   [D,D,R,L],
   [U,U,D,D],
   [R,L,U,U],
   [D,D,R,L],
   [U,U,R,L]]
   [2,2,2,2]
   [1,1,1,2,1,1,1]
   [2,2,2,2]
   [1,1,1,2,1,1,1])
   10
   ]


-- Komunikat wygenerowany po wywolaniu funkcji main
-- ======================
-- Test: 1/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 2/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 3/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 4/9
-- CPU time:  73.51s
-- Accept!
-- ======================
-- Test: 5/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 6/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 7/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 8/9
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 9/9
-- CPU time:   0.00s
-- Accept!
-- Accepted: 9/9-- 
-- 

-- ======================
-- Test: 1/5
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 2/5
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 3/5
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 4/5
-- CPU time:   0.00s
-- Accept!
-- ======================
-- Test: 5/5
-- CPU time:   0.00s
-- Accept!
-- Accepted: 5/5

main :: IO ()
main = checkerMain solve tests
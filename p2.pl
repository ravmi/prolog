% Rafal Michaluk 360179

%listaBezPowtorzen(+L)
listaBezPowtorzen([], _).
listaBezPowtorzen([X|T], A) :- \+ member(X, A), listaBezPowtorzen(T, [X|A]).
listaBezPowtorzen(L) :- listaBezPowtorzen(L, []).

%bezPowtorzen(+L1, -L2)
% L2 to lista L1 z usunietymi powtorzeniami
bezPowtorzen([], []).
bezPowtorzen([X|T1], [X|T2]) :- \+ member(X, T1), bezPowtorzen(T1, T2).
bezPowtorzen([X|T1], T2) :- member(X, T1), bezPowtorzen(T1, T2).

%wierzcholki(+G, -Ws)
% Ws to lista wierzcholkow z grafu G
wierzcholki([], []).
wierzcholki([node(V, _, _)|T], [V|L]) :- wierzcholki(T, L).

%krawedzie(+V, +K, -Kr)
% tworzy krawedzie z V to elementow listy K
krawedzie(_, [], []).
krawedzie(V, [X|R], [kr(V, X)|RKr]) :- krawedzie(V, R, RKr).

%fKrawedzie(+G, -R)
% wszystkie fKrawedzie grafu G
fKrawedzie([], []).
fKrawedzie([node(V, _, F)|Ws], R) :- krawedzie(V, F, Kr1), fKrawedzie(Ws, Kr2), append(Kr1, Kr2, R).

%odwrocKrawedzie(+L1, -L2)
odwrocKrawedzie([], []).
odwrocKrawedzie([kr(X, Y)|T], [kr(Y, X)|U]) :- odwrocKrawedzie(T, U).

%zawarty(+L1, +L1)
% elementy L1 zawarte w L2
zawarty([], _).
zawarty([X|R], L) :- member(X, L), zawarty(R, L).

%fKrawedzieOK(G)
% kazda krawedz v-w ma odpowiednik w-v
fKrawedzieOK(G) :- fKrawedzie(G, FKr), odwrocKrawedzie(FKr, FKrRev), zawarty(FKr, FKrRev), zawarty(FKrRev, FKr).

%wierzcholkiOK(+G)
% wierzcholki sie nie powtarzaja
wierzcholkiOK(G) :- wierzcholki(G, W), listaBezPowtorzen(W).

%krawedzieOK(+Graf, +ListaWierzcholkow)
% Sprawdza czy dla kazdego node(_, E, F) w G, E i F sa zawarte w ListaWierzcholkow.
krawedzieOK([], _).
krawedzieOK([node(_, E, F)|T], W) :- zawarty(E, W), zawarty(F, W), krawedzieOK(T, W).
%krawedzieOK(+G)
krawedzieOK(G) :- wierzcholki(G, W), krawedzieOK(G, W).

%jestEFGrafem(+G)
jestEFGrafem(G) :- wierzcholkiOK(G), fKrawedzieOK(G), krawedzieOK(G).

%jestWierzcholkiem(+G, -V)
jestWierzcholkiem([node(V, _, _)|_], V).
jestWierzcholkiem([_|T], V) :- jestWierzcholkiem(T, V).

%maEWchodzace(+V, -G)
maEWchodzace(V, [node(_, E, _)|_]) :- member(V, E).
maEWchodzace(V, [_|T]) :- maEWchodzace(V, T).

%jestPoczatkiem(+V, -G)
jestPoczatkiem(V, G) :- jestWierzcholkiem(G, V), \+ maEWchodzace(V, G).

%maEWychodzace(+V, -G)
maEWychodzace(V, [node(V, [_|_], _)|_]).
maEWychodzace(V, [_|T]) :- maEWychodzace(V, T).

%jestKoncem(+V, -G)
jestKoncem(V, G) :- jestWierzcholkiem(G, V), \+ maEWychodzace(V, G).

%usun(-V, -L1, +L2)
usun(_, [], []).
usun(V, [V|T], X) :- usun(V, T, X).
usun(V, [X|T], [X|Y]) :- usun(V, T, Y).

%eSasiedzi(-V, -G, +ESasiedzi)
eSasiedzi(V, [node(V, E, _)|_], E).
eSasiedzi(V, [_|T], N) :- eSasiedzi(V, T, N).

%fSasiedzi(-V, -G, +FSasiedzi)
fSasiedzi(V, [node(V, _, F)|_], F).
fSasiedzi(V, [_|T], N) :- fSasiedzi(V, T, N).

%wierzcholek3FSasiedzi(+V, +G)
wierzcholek3FSasiedzi(V, G) :- fSasiedzi(V, G, F), bezPowtorzen(F, Fu), length(Fu, Len), Len =< 3.
wierzcholki3FSasiedzi([], _).
%wierzcholki3FSasiedzi(+L, +G)
wierzcholki3FSasiedzi([X|T], G) :- wierzcholek3FSasiedzi(X, G), wierzcholki3FSasiedzi(T, G).

%os(+V, +W, +G)
% czy W jest osiagalny z V w grafie G (po Ekrawedziach)
os(V, W, G) :- os(V, W, G, [V]).
os(V, W, G, _) :- eSasiedzi(V, G, S), member(W, S).
os(V, W, G, Visited) :- eSasiedzi(V, G, S), member(U, S), \+ member(U, Visited), os(U, W, G, [U|Visited]).

%polaczone(+V, +W, +G)
% czy W jest osiagalny z V lub V z W w grafie G (po E krawedziach)
polaczone(V, W, G) :- os(V, W, G).
polaczone(V, W, G) :- os(W, V, G).

%polaczoneWierzcholekLista(-V, -L, -G)
% czy wierzcholek V jest polaczony z kazdym elementem listy L
polaczoneWierzcholekLista(_, [], _).
polaczoneWierzcholekLista(V, [V|T], G) :- polaczoneWierzcholekLista(V, T, G).
polaczoneWierzcholekLista(V, [W|T], G) :- polaczone(V, W, G), polaczoneWierzcholekLista(V, T, G).

%polaczoneListaLista(-L1, -L2, G)
% czy kazdy element z L1 jest polaczony z kazdym elementem z l2 w grafie G
polaczoneListaLista([], _, _).
polaczoneListaLista([V|T], L2, G) :- polaczoneWierzcholekLista(V, L2, G), polaczoneListaLista(T, L2, G).

%jestDobrzeUlozony(-G)
jestDobrzeUlozony(G) :- jestEFGrafem(G), jestPoczatkiem(A, G), jestKoncem(B, G), \+ A = B, wierzcholki(G, W), wierzcholki3FSasiedzi(W, G), polaczoneListaLista(W, W, G).

%istniejeUA(+P, +G)
% istnieje wierzcholek u, opisany w pierwszym warunku na graf dobrze permutujacy
istniejeUA(pair(V1, W1), G) :- fSasiedzi(V1, G, S), member(U, S), ePoprzednicy(U, G, S2), member(W1, S2).
dlaWszystkichIstniejeUA([], _).
dlaWszystkichIstniejeUA([X|T], G) :- istniejeUA(X, G), dlaWszystkichIstniejeUA(T, G).

%istniejeUB(+P, +G)
% istnieje wierzcholek u, opisany w drugim warunku na graf dobrze permutujacy
istniejeUB(pair(V1, W1), G) :- fSasiedzi(V1, G, S), member(U, S), eSasiedzi(U, G, P), member(W1, P).
dlaWszystkichIstniejeUB([], _).
dlaWszystkichIstniejeUB([X|T], G) :- istniejeUB(X, G), dlaWszystkichIstniejeUB(T, G).

%nieJestESasiadem(+V, +G).
% Zadna krawedz E nie wchodzi do V
nieJestESasiadem(_, []).
nieJestESasiadem(V, [node(_, E, _)|T]) :- \+ member(V, E), nieJestESasiadem(V, T).

%ePoprzednicy(+V, +G, -L).
% L to wierzcholki wchodzace do V po Ekrawedziach w grafie G
ePoprzednicy(_, [], []).
ePoprzednicy(V, [node(S, E, _)|T], [S|R]) :- member(V, E), ePoprzednicy(V, T, R).
ePoprzednicy(V, [node(_, E, _)|T], R) :- \+ member(V, E), ePoprzednicy(V, T, R).

%wierzcholekPermutujacyA(+V, +G, +Koniec)
% wierzcholek V spelnia pierwszy warunek na graf dobrze permutujacy
wierzcholekPermutujacyA(V, G, _) :- eSasiedzi(V, G, []).
wierzcholekPermutujacyA(V, G, Koniec) :- fSasiedzi(V, G, [Koniec]).
wierzcholekPermutujacyA(V, G, _) :- fSasiedzi(V, G, []).
wierzcholekPermutujacyA(V, G, Koniec) :- eSasiedzi(V, G, L), fSasiedzi(V, G, L2), usun(L2, Koniec, L3), allPairs(L, L3, R), dlaWszystkichIstniejeUA(R).

%wierzcholekPermutujacyB(+V, +G, +Poczatek)
% wierzcholek V spelnia drugi warunek na graf dobrze permutujacy
wierzcholekPermutujacyB(V, G, _) :- nieJestESasiadem(V, G).
wierzcholekPermutujacyB(V, G, Poczatek) :- fSasiedzi(V, G, [Poczatek]).
wierzcholekPermutujacyB(V, G, _) :- fSasiedzi(V, G, []).
wierzcholekPermutujacyB(V, G, Poczatek) :- ePoprzednicy(V, G, L), fSasiedzi(V, G, L2), usun(L2, Poczatek, L3), allPairs(L, L3, R), dlaWszystkichIstniejeUB(R).

%wierzcholekPermutujacy(+V, +G)
% wierzcholek spelnia warunki na graf dobrze permutujacy
wierzcholekPermutujacy(V, G) :- jestPoczatkiem(P, G), jestKoncem(K, G), wierzcholekPermutujacyA(V, G, K), wierzcholekPermutujacyB(V, G, P).

%wierzcholkiDobrzePermutuja(+L, +G)
% wierzcholki z listy L spelniaja warunek na graf dobrze permutujacy
wierzcholkiDobrzePermutuja([], _).
wierzcholkiDobrzePermutuja([V|T], G) :- wierzcholekPermutujacy(V, G), wierzcholkiDobrzePermutuja(T, G).

%jestDobrzePermutujacy(+G)
jestDobrzePermutujacy(G) :- jestDobrzeUlozony(G), wierzcholki(G, L), wierzcholkiDobrzePermutuja(L, G).

%idzF(+V, +G, +Odwiedzone, -Sciezka)
% znajduje sciezke w grafie G zaczynajaca sie w wierzcholku V, po krawedziach typu F.
idzF(V, _, _, [V]).
idzF(V, G, Odwiedzone, [V|X]) :- fSasiedzi(V, G, S), member(W, S), \+ member(W, Odwiedzone), idzF(W, G, [W|Odwiedzone], X).
%fSciezka(+G, -F)
% znajduje F-sciezki w grafie G
fSciezka(G, F) :- wierzcholki(G, Vs), member(V, Vs), idzF(V, G, [V], F).

%fSciezkaMniejsza(+L1, +L2, +G)
% czy sciezka L1 jest mniejsza od sciezki L2
fSciezkaMniejsza([], _, _).
fSciezkaMniejsza([X|R1], [Y|R2], G) :- eSasiedzi(X, G, N), member(Y, N), fSciezkaMniejsza(R1, R2, G).

%jestSucc(+G, -L1, -L2)
jestSucc(_, [], []).
jestSucc(G, [], S) :- fSciezka(G, S).
jestSucc(G, A, B) :- fSciezka(G, A), fSciezka(G, B), fSciezkaMniejsza(A, B, G).

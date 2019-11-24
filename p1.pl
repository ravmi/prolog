% Rafał Michaluk 360179
%
% Przez 'choice' rozumiem wybór, jak w treści zadania
% Użyłem angielskich nazw i dokumentacji, opakowując je w funkcje o polskich nazwach.
% Funkcje o polskich nazwach są na samym dole. 
% jestWyborem działa dla każdej reprezentacji z tym samym wierzchołkiem poczatkowym
% (permutacja wierszy i kolumn), więc jestADFS daje dużo powtórzonych rozwiązań
% (dla każdej reprezentacji).

% ---------------------------------------------------- %
%
% permutate(L1, L2)
% success, when L2 is a permutation of L1
permutate([], []).
permutate(L1, [X|Rest]) :-
    member(X, L1), subtract(L1, [X], L1mX), permutate(L1mX, Rest).


% nodeChoice(+NodeRepresentation, -ChosenNeighbors).
% success, when ChosenNeighbors is a list of Neighbors
% of the node represented by NodeRepresentation in some
% choice of a graph
nodeChoice([V, a | L], [V, a | Lp]) :-
    permutate(L, Lp).
nodeChoice([V, e | L], [V, e, X]) :-
    member(X, L).


% isChoiceRest(+AEG, -G)
% success, when G is a choice from AEG and nodes representations
% are in the same order.
isChoiceRest([], []).
isChoiceRest([NodeRepr|G], [ChosenNodeRepr|ChosenGRest]) :-
    nodeChoice(NodeRepr, ChosenNodeRepr),
    isChoiceRest(G, ChosenGRest).


% isChoice(+AEG, -G)
% success, when G is a choice from AEG
isChoice([], []).
isChoice([N1|Rest], [N2|Result]) :-
    nodeChoice(N1, N2),
    isChoiceRest(Rest, Choice), permutate(Choice, Result).


% subtract(List1, List2, List3)
% success, when after removing List2 elements from List1 we acquire List3
subtract([], _, []).
subtract([X|L1], L2, [X|Result]) :-
    \+member(X, L2), subtract(L1, L2, Result).
subtract([X|L1], L2, Result) :-
    member(X, L2), subtract(L1, L2, Result).


% neighborsAE(+G, +V, -Neighbors)
% success, when Neighbors is a list of neighbors of the node V in graph G
neighborsAE(G, V, Neighbors) :-
    member([V, _|Neighbors], G).


% neighborsAEChoice(+G, +V, -Neighbors)
% success, when Neighbors is a list of neighbors of the node V
% in some choice of a graph G
neighborsAEChoice(G, V, Neighbors) :-
    member([V, a |Neighbors], G).
neighborsAEChoice(G, V, [Neighbor]) :-
    member([V, e |Neighbors], G), member(Neighbor, Neighbors).


% isDFSNode(+G, +V, +Visited, -ListDFS)
% success, when ListDFS is a list of identifiers of nodes
% in order visited by DFS algorithm starting in V node in the graph G
% after already visiting Visited nodes
isDFSNode(G, V, Visited, [V|ListDFS]) :- 
    neighborsAE(G, V, Neighbors),
    isDFSList(G, Neighbors, [V|Visited], ListDFS).

% isDFSList(+G, Neighbors, +Visited, -ListDFS)
% success, when ListDFS is a list of identifiers of nodes
% in order visited by DFS algorithm starting in one of nodes from Nodes
% (and traversing every node from Nodes)
% in the graph G after already visiting Visited nodes
isDFSList(G, Nodes, Visited, Result) :-
    subtract(Nodes, Visited, Unvisited),
    % choosing next node (V) we are going to visit, using member gives
    % every possible order in Nodes
    member(V, Unvisited),
    % recursive DFS starting from the node V
    isDFSNode(G, V, Visited, Result1),
    append(Result1, Visited, Visited2),
    % visiting reminding unvisited nodes from Nodes
    isDFSList(G, Unvisited, Visited2, Result2),
    % merging both results
    append(Result1, Result2, Result).
isDFSList(_, Nodes, Visited, []) :- subtract(Nodes, Visited, []).

% isDFS(+G, -List)
% success, when List is a list of identifiers of nodes
% if order visited by DFS algorithm starting in first node of the graph G
isDFS([[V|L1]|LL2], ListDFS) :-
    isDFSNode([[V|L1]|LL2], V, [], ListDFS).


% isDFSNodeChoice(+G, +V, +Visited, -ListDFS)
% success, when ListDFS is a list of identifiers of nodes
% in order visited by DFS algorithm starting in V node in some choice of the graph G
% after already visiting Visited nodes
isDFSNodeChoice(G, V, Visited, [V|ListDFS]) :- 
    neighborsAEChoice(G, V, Neighbors),
    isDFSListChoice(G, Neighbors, [V|Visited], ListDFS).

% isDFSListChoice(+G, Neighbors, +Visited, -ListDFS)
% success, when ListDFS is a list of identifiers of nodes
% in order visited by DFS algorithm starting in one of nodes from Nodes
% (and traversing every node from Nodes)
% in some choice of the graph G after already visiting Visited nodes
isDFSListChoice(G, Nodes, Visited, Result) :-
    subtract(Nodes, Visited, Unvisited),
    % choosing next node (V) we are going to visit, using member gives
    % every possible order in Nodes
    member(V, Unvisited),
    % recursive DFS starting from the node V
    isDFSNodeChoice(G, V, Visited, Result1),
    append(Result1, Visited, Visited2),
    % visiting reminding unvisited nodes from Nodes
    isDFSListChoice(G, Unvisited, Visited2, Result2),
    % merging both results
    append(Result1, Result2, Result).
isDFSListChoice(_, Nodes, Visited, []) :- subtract(Nodes, Visited, []).

% isDFS(+G, -List)
% success, when List is a list of identifiers of nodes
% if order visited by DFS algorithm starting in first node of some choice of the graph G
isDFSChoice([[V|L1]|LL2], ListDFS) :-
    isDFSNodeChoice([[V|L1]|LL2], V, [], ListDFS).


% jestWyborem(+AEgraf, -Graf)
% odnosi sukces, gdy Graf jest wyborem z AEgraf. 
jestWyborem(X, Y) :- isChoice(X, Y).


% jestDFS(+Graf, -Lista)
% odnosi sukces, gdy Lista jest listą identyfikatorów wierzchołków
% kolejno odwiedzanych przez algorytm przechodzenia grafu Graf w głąb
% przy przejsciu startującym z pierwszego wierzchołka tego grafu
jestDFS(G, ListDFS) :-
    isDFS(G, ListDFS).


% jestADFS(+AEgraf, -Lista)
% odnosi sukces, gdy Lista jest listą identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia
% w głąb przy przejsciu przez pewien graf będący wyborem z AEgraf.
% Korzysta z predykatu jestDFS.
jestADFS(G, R) :-
    jestWyborem(G, G1), jestDFS(G1, R).


% jestADFS1(+AEgraf, -Lista)
% odnosi sukces, gdy Lista jest listą identyfikatorów
% wierzchołków kolejno odwiedzanych przez algorytm przechodzenia
% w głąb przy przejsciu przez pewien graf będący wyborem z AEgraf.
% Nie korzysta z predykatu jestDFS.
jestADFS1(G, R) :-
    isDFSChoice(G, R).

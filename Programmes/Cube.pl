get_primes(L, H, []) :-
    H < L.
get_primes(L, H, [L|R]) :-
    H >= L,
    prime(L),
    L1 is L + 1,
    get_primes(L1, H, R).
get_primes(L,H, R) :-
    H >= L,
   \+ prime(L),
    L1 is L + 1,
    get_primes(L1, H, R).

prime(2).
prime(N) :-
  N > 2, \+ factorisable(2,N).

factorisable(F, N) :-
  0 is N mod F.
factorisable(F, N) :-
  F + 1 < N,
  F1 is F + 1,
  factorisable(F1, N).

unique_digits(N) :-
    digits(N, D),
    is_set(D).

digits( N, [N] ) :-
  N < 10.
digits( N, W ) :-
    N >= 10, 
    div_mod( N, 10, D, M ),
    digits( D, R ),
    append( R, [M], W ).

div_mod( A, B, D, M ) :-
    D is A div B,
    M is A mod B.

prime_splits(PS, S) :-
  prime_splits_(PS, [0,1,2,3,4,5,6,7,8,9], S).
prime_splits_([P|_], DS, [P]) :-
  subset(P, DS),
  subtract(DS, P, W),
  W == [].
prime_splits_([P|PS], DS, [P|R]) :-
  subset(P, DS),
  subtract(DS, P, W),
  \+ W == [],
  prime_splits_(PS, W, R).
prime_splits_([_|PS], DS, S) :-
  prime_splits_(PS, DS, S).

generator4(T) :-
	get_primes(2, 9851, P),
	include(unique_digits, P, FP),
	maplist(digits, FP, PS),
	prime_splits(PS, S),
 	permutation(S,T).

map_digits([], []).
map_digits([X|XS], R) :-
    digits(X, D),
    map_digits(XS, W),
    append(D, W, R).

cube(N) :- 
    nth_integer_root_and_remainder(3, N, _, 0).

cube_splits([]).
cube_splits(X) :-
    divide(X, A, B),
    makes_cube(A),
    cube_splits(B).

makes_cube([H|T]) :-
    digits_to_int([H|T], N),
    cube(N).

reverse_list([], []).
reverse_list([X|XS], R) :-
    reverse_list(XS, ReversedRest),
    append(ReversedRest, [X], R).

digits_to_int( D, N ) :-
  digits_to_int( D, 0, N ).
digits_to_int( [], X, X ).
digits_to_int( [H|T], X, N ) :-
  W is X * 10 + H,
  digits_to_int( T, W, N ).

insert_list([], E, [E]).
insert_list([X|XS], E, [E,X|XS] ) :-
    E =< X.
insert_list([X|XS], E, [X|Rest]) :-
    E > X,
    insert_list(XS, E, Rest).

sort_list([], []).
sort_list([H|T], R) :-
  sort_list(T, W),
  insert_list(W, H, R).

tester4(T) :-
  maplist(digits_to_int, T, P),
  sort_list(P, [_|N]),
  reverse_list(N, R),
  map_digits(R, D),
  digits_to_int(D, I),
  digits(I, C),
  cube_splits(C).

divide(L, A, B) :-
append(A, B, L),
length(A, X), X =< 4, 1 =< X.

%TESTERS%
x_generator4( N ) :-
  x_generator4_loop(
    [ [[9 ,6 ,7] ,[4 ,0 ,1] ,[2 ,8 ,3] ,[5]]
    , [[9 ,8 ,3] ,[6 ,0 ,1] ,[5] ,[4 ,7] ,[2]]
    , [[9 ,8 ,3] ,[6 ,7] ,[4 ,2 ,0 ,1] ,[5]]
    , [[9 ,8 ,5 ,1] ,[2] ,[4 ,3] ,[6 ,0 ,7]]
    , [[9 ,8 ,5 ,1] ,[2] ,[3] ,[6 ,0 ,4 ,7]]
    , [[9 ,8 ,5 ,1] ,[2] ,[7] ,[4 ,6 ,0 ,3]]
    , [[8 ,9] ,[7] ,[6 ,0 ,1] ,[2 ,5 ,4 ,3]]
    , [[8 ,9] ,[7] ,[5 ,6 ,3] ,[4 ,0 ,2 ,1]]
    , [[8 ,9] ,[5] ,[4 ,7] ,[6 ,0 ,1] ,[3] ,[2]]
    , [[3] ,[5] ,[6 ,0 ,7] ,[2] ,[4 ,1] ,[8 ,9]] ], 0, N ).
    
x_generator4_loop( [], C, C ).
x_generator4_loop( [T|TS], C, N ) :-
  generator4( T ),
  C1 is C + 1,
  x_generator4_loop( TS, C1 , N ).
x_generator4_loop( [_|TS], C, N ) :-
  x_generator4_loop( TS , C, N ).

x_tester4( N ) :-
  x_tester4_loop(
    [ [[8 ,2 ,7] ,[6 ,1] ,[5 ,3] ,[4 ,0 ,9]]
    , [[8 ,2 ,7] ,[6 ,1] ,[4 ,0 ,9] ,[5 ,3]]
    , [[8 ,2 ,7] ,[5 ,3] ,[6 ,1] ,[4 ,0 ,9]]
    , [[8 ,2 ,7] ,[4 ,0 ,9] ,[6 ,1] ,[5 ,3]]
    , [[6 ,1] ,[8 ,2 ,7] ,[4 ,0 ,9] ,[5 ,3]]
    , [[6 ,1] ,[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7]]
    , [[5 ,3] ,[6 ,1] ,[4 ,0 ,9] ,[8 ,2 ,7]]
    , [[5 ,3] ,[4 ,0 ,9] ,[6 ,1] ,[8 ,2 ,7]]
    , [[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7] ,[6 ,1]]
    , [[4 ,0 ,9] ,[8 ,2 ,7] ,[6 ,1] ,[5 ,3]] ], 0, N ).
x_tester4_loop( [], C, C ).
x_tester4_loop( [T|TS], C, N ) :-
  tester4( T ),
  C1 is C + 1,
  x_tester4_loop( TS , C1 , N ).
x_tester4_loop( [_|TS], C, N ) :-
  x_tester4_loop( TS , C, N ).

main :- generator4(P), tester(P), write(P).
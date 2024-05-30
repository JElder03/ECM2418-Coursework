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

different( [] ).
different( [H|T] ) :-
    \+ member( H, T ),
    different( T ).

get_elem(0, [H|_], H).
get_elem(N, [_|T], W) :-
  N1 is N - 1,
  get_elem(N1, T, W).

member_list([E|_], E).
member_list([_|XS], E) :- member_list(XS, E).

generator3( T ) :-
  between(32, 1000, S),
  T is S ^ 2.

tester3( T ) :-
  digits( T, TS ),
  different( TS ),
  length( TS, I ),
  I1 is I - 1,
  get_elem( I1, TS, L1 ),
  L1 =:= I,
  I2 is I1 - 1,
  get_elem( I2, TS, L2 ),
  1 is L2 mod 2,
  member_list( TS, 0 ),
  get_elem( 0, TS, F1 ),
  0 is L2 mod F1,
  get_elem( 1, TS, F2 ),
  0 is F2 mod F1,
  get_elem( 2, TS, F3 ),
  0 is F3 mod F1,
  \+ member_list([F2,F3,L2], 0 ).
  
% TESTERS %
x_generator3( N ) :-
  x_generator3_loop(
    [ 1024 , 9409 , 23716 , 51529
    , 123904 , 185761 , 868624 , 962361
    , 982081 , 1000000 ], 0, N ).

x_generator3_loop( [], C, C ).
x_generator3_loop( [T|TS], C, N ) :-
  generator3( T ),
  C1 is C + 1,
  x_generator3_loop( TS , C1 , N ).
x_generator3_loop( [_|TS], C, N ) :-
  x_generator3_loop( TS , C, N ).

x_tester3( N ) :-
  x_tester3_loop(
    [ 123056 , 128036 , 139076 , 142076
    , 148056 , 159076 , 173096 , 189036
    , 193056 , 198076 ], 0, N ).

x_tester3_loop( [], C, C ).
x_tester3_loop( [T|TS], C, N ) :-
  tester3( T ),
  C1 is C + 1,
  x_tester3_loop( TS , C1 , N ).

main :- generator3( N ), tester3( N ), write( N ).
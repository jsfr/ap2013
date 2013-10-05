%%%---------------------------------------------------------------------
%%% @authors: Michael Kirkedal Thomsen <shapper@diku.dk>
%%%           Erik Partridge 
%%% @copyright (C) 2013, Michael Kirkedal Thomsen, Erik Partridge
%%% Created : Aug 2013 for exercise in Advanced Programming 2013
%%%---------------------------------------------------------------------
%%% Student: Jens Fredskov
%%% KU-Id:   chw752
%%%---------------------------------------------------------------------
upd :- consult('nats.pl').

%%%---------------------------------------------------------------------
%%% Part 1:
%%%---------------------------------------------------------------------

% less (X,Y) => X < Y
less(z, s(_)).
less(s(X), s(Y)) :-
    less(X, Y).

% add (X,Y,Z) => Z = X + Y
add(z, z, z).
add(s(X), Y, s(Z)) :-
    add(X, Y, Z).
add(z, s(Y), s(Z)) :-
    add(z, Y, Z).

% mult(X,Y,Z) => Z = X * Y
mult(z, z, z).
mult(z, s(_), z).
mult(s(_), z, z).
mult(s(X), Y, Z) :-
    less(X, Z),
    less(Y, s(Z)),
    mult(X, Y, K),
    add(Y, K, Z).

% mod(X,Y,Z) => Z = X mod Y.
mod(X, X, z).
mod(X, Y, X) :-
    less(X, Y).
mod(X, Y, Z) :-
    less(Y, X),
    add(Y, K, X),
    mod(K, Y, Z).

%%%---------------------------------------------------------------------
%%% Part 2:
%%%---------------------------------------------------------------------

% notprime(X) => X is not a prime
notprime(z).
notprime(s(z)).
notprime(X) :-
    less(K,X),
    less(s(z),K),
    mult(K, Kd, X),
    less(K, s(Kd)).

% notdividable(X, Y) => X is not dividable with any number from Y to X-1
notdividable(X, X).
notdividable(X, Y) :-
    mod(X, Y, s(_)),
    notdividable(X, s(Y)).

% prime(X) => X is a prime
prime(X) :-
    less(s(z), X),
    notdividable(X,s(s(z))).

% listPrimes(L,X) => L is the list of primes small than or equal to X
listPrimes([], z).
listPrimes([s(X) | L] , s(X)) :-
    prime(s(X)),
    listPrimes(L, X).
listPrimes(L, s(X)) :-
    notprime(s(X)),
    listPrimes(L, X).

% listLength(L, I) => I is the length of list L
listLength([], z).
listLength([_ | T], s(I)) :-
    listLength(T, I).

% superprime(X) => X is a super-prime
superprime(X) :-
    prime(X),
    listPrimes(L, X),
    listLength(L, I),
    prime(I).

%%%---------------------------------------------------------------------
%%% Tests: 
%%%---------------------------------------------------------------------

less_test(B1, B2) :-
    i2n(B1, U1),
    i2n(B2, U2),
    less(U1, U2).

add_test(B1, B2, B3) :-
    i2n(B1, U1),
    i2n(B2, U2),
    add(U1, U2, U3),
    n2i(U3, B3).

mult_test(B1, B2, B3) :-
    i2n(B1, U1),
    i2n(B2, U2),
    mult(U1, U2, U3),
    n2i(U3, B3).

mod_test(B1, B2, B3) :-
    i2n(B1, U1),
    i2n(B2, U2),
    mod(U1, U2, U3),
    n2i(U3, B3).

notprime_test(B) :-
    i2n(B, U),
    notprime(U).

prime_test(B) :-
    i2n(B, U),
    prime(U).

listPrimes_test(LB, B) :-
    i2n(B,U),
    listPrimes(LU, U),
    n2i_list(LU, LB).

superprime_test(B) :-
    i2n(B, U),
    superprime(U).

%%%---------------------------------------------------------------------
%%% Help of convert to and from unary numbers
%%%---------------------------------------------------------------------

% Binary to unary
i2n(X,s(Y)) :- X > 0, Z is (X-1), i2n(Z,Y).
i2n(0,z).

% Unary to binary
n2i(s(X),Y) :- n2i(X,Z), Y is (Z+1).
n2i(z,0).

% Unary list to binary list
n2i_list([], []).
n2i_list([U | LU], [B | LB]) :-
    n2i(U, B),
    n2i_list(LU, LB).
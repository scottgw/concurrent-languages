%
% randmat: random number generation
%
% input:
%   nrows, ncols: the number of rows and columns
%   s: the seed
%
% output:
%   matrix: a nrows x ncols integer matrix
%

-module(randmat).
-export([randmat/3]).
-define(INT_MAX,4294967296).
-define(RAND_MAX,100).
-define(LCG_A,1664525).
-define(LCG_C,1013904223).

randvet(0, _) -> [];
randvet(Ncols, S) ->
  NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
  [NewS rem ?RAND_MAX | randvet(Ncols - 1, NewS)].

randvet_acc (0, _, Acc) ->
    lists:reverse (Acc);
randvet_acc (Ncols, S, Acc) ->
    NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
    Val = NewS rem ?RAND_MAX,
    randvet_acc (Ncols - 1, NewS, [Val] ++ Acc).


randmat(Nrows, Ncols, S) ->
    randmat_acc (Nrows, Ncols, S, []).

randmat_acc (0, _, _, Acc) -> lists:reverse (Acc);
randmat_acc (Nrows, Ncols, S, Acc) -> 
    randmat_acc (Nrows - 1, Ncols, S+1, [randvet (Ncols, S)] ++ Acc).

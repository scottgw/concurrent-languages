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

-module(main).
-export([main/0, main/1]).
-export([row_proc/3]).
-define(INT_MAX,4294967296).
-define(RAND_MAX,100).
-define(LCG_A,1664525).
-define(LCG_C,1013904223).

randvet(Ncols, S) ->
    randvet_acc (Ncols, S, []).

randvet_acc (0, _, Acc) ->
    lists:reverse (Acc);
randvet_acc (Ncols, S, Acc) ->
    NewS = (?LCG_A * S + ?LCG_C) rem ?INT_MAX,
    Val = NewS rem ?RAND_MAX,
    randvet_acc (Ncols - 1, NewS, [Val] ++ Acc).

join_acc ([], Acc) -> lists:reverse (Acc);
join_acc ([P|Pids], Acc) -> 
    join_acc (Pids, [receive Result -> Result end] ++ Acc).

row_proc(Parent, Ncols, S) ->
    Parent ! randvet (Ncols, S).

randmat(Nrows, Ncols, S) -> join_acc (randmat_acc (Nrows, Ncols, S, []), []).

randmat_acc (0, _, _, Acc) -> lists:reverse (Acc);
randmat_acc (Nrows, Ncols, S, Acc) ->
    randmat_acc (Nrows - 1, Ncols, S + 1,
                 [spawn(?MODULE, row_proc, [self(), Ncols, S]) | Acc]).

main() -> main(['']).
main(Args) ->
  [Head | _] = Args,
  IsBench = string:equal(Head, 'is_bench'),
  {ok, [Nrows, Ncols, S]} = io:fread("","~d~d~d"),
  Matrix = randmat(Nrows, Ncols, S),
  case IsBench of false -> io:format("~w~n\n", [Matrix]); true -> ''
  end.

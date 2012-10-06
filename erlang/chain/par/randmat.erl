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

join(Pids) -> [receive {Pid, Result} -> Result end || Pid <- Pids].

randmat(Nrows, Ncols, S) ->
  Parent = self(),
  % parallel_for on rows
  join([spawn(fun() -> Parent ! {self(), randvet(Ncols, S + Row - 1)} end) ||
      Row <- lists:seq(1, Nrows)]).

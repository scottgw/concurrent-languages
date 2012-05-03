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
-define(INT_MAX,1000).

randvet_impl(0) -> [];
randvet_impl(Ncols) -> [random:uniform(?INT_MAX) | randvet_impl(Ncols - 1)].

randvet(Ncols, S) ->
  random:seed(S + pid_to_integer(self()), S, S),
  Vet = randvet_impl(Ncols),
  receive
    {From} ->
      From ! {Vet}
  end.

pid_to_integer(X) ->
  Y = pid_to_list(X),
  W = string:substr(Y, string:str(Y, ".") + 1, string:len(Y)),
  Z = string:substr(W, 1, string:str(W, ".") - 1),
  list_to_integer(Z).

randmat_impl(0, _, _) -> [];
randmat_impl(Nrows, Ncols, S) ->
  [spawn(fun() -> randvet(Ncols, S) end) |
    randmat_impl(Nrows - 1, Ncols, S)].

send_self([]) -> [];
send_self([X|Rest]) -> [(X ! {self()}) | send_self(Rest)].

join(0) -> [];
join(Nrows) ->
  receive
    {Vet} ->
      [Vet | join(Nrows - 1)]
  end.

randmat(Nrows, Ncols, S) ->
  All = randmat_impl(Nrows, Ncols, S),
  send_self(All),
  join(Nrows).
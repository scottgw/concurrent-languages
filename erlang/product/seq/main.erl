%
% product: a matrix-vector product
%
% input:
%   nelts: the number of elements
%   matrix: a real matrix
%   vector: a real vector
%
% output:
%   result: a real vector, whose values are the result of the product
%

-module(main).
-export([main/0, main/1]).

product(_, Matrix, Vector) ->
  [ lists:sum([ A * B || {A, B} <- lists:zip(L, Vector)]) || L <- Matrix].

read_vector(IsBench, 0) -> [];
read_vector(IsBench, Nelts) -> 
    Val = if IsBench -> 0;
             true -> {ok, [X]} = io:fread("", "~f"), X
          end,
    [ Val | read_vector(IsBench, Nelts - 1)].

read_matrix(IsBench, 0, _) -> [];
read_matrix(IsBench, Nelts, Total) ->
  [ read_vector(IsBench, Total) | read_matrix(IsBench, Nelts - 1, Total)].


main() -> main(['']).
main(Args) ->
  [Head | _] = Args,
  IsBench = string:equal(Head, 'is_bench'),
  {ok, [Nelts]} = io:fread("","~d"),
  Matrix = read_matrix(IsBench, Nelts, Nelts),
  Vector = read_vector(IsBench, Nelts),
  io:format("~w~n\n", [product(Nelts, Matrix, Vector)]).


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

join(Pids) ->
  [receive {Pid, Result} -> Result end || Pid <- Pids].

prod_row ([], [], Acc) ->
    Acc;
prod_row ([R|Row], [V|Vector], Acc) ->
    prod_row (Row, Vector, Acc + (R*V)).

product(_, Matrix, Vector) ->
  Parent = self(),
  % parallel for on rows
  join([spawn(fun() -> Parent ! {self(), prod_row (L, Vector,0)} end)
        || L <- Matrix]).

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
    if IsBench -> '';
       true -> io:format("~w~n\n", [product(Nelts, Matrix, Vector)])
    end.

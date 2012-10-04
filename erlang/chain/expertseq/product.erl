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

-module(product).
-export([product/3]).

prod_row ([], [], Acc) ->
    Acc;
prod_row ([R|Row], [V|Vector], Acc) ->
    prod_row (Row, Vector, Acc + (R*V)).

product (_, Matrix, Vector) ->
    lists:map (
      fun (Row) -> prod_row (Row, Vector, 0) end,
      Matrix).

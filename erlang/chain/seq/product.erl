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

product(_, Matrix, Vector) ->
  [ lists:sum([ A * B || {A, B} <- lists:zip(L, Vector)]) || L <- Matrix].

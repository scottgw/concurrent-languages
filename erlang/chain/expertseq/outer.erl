%
% outer: outer product
%
% input:
%   points: a vector of (x, y) points
%   nelts: the number of points
%
% output:
%   matrix: a real matrix, whose values are filled with inter-point
%     distances
%   vector: a real vector, whose values are filled with origin-to-point
%     distances
%

-module(outer).
-export([outer/2]).

sqr(X) ->
  X * X.

distance({Ax, Ay}, {Bx, By}) ->
  math:sqrt(sqr(Ax - Bx) + sqr(Ay - By)).

fix_diagonal_vector(_, _, [], _, _, _) -> [];
fix_diagonal_vector(Line, Col, [Head | Tail], Point, Nelts, Nmax) ->
  if Line == Col -> [Nelts * Nmax | Tail];
    true -> [ Head | fix_diagonal_vector(
          Line, Col + 1, Tail, Point, Nelts, Nmax)]
  end.

fix_diagonal (Matrix, Points, Nelts) ->
    lists:zipwith3 
      (fun (Line, Vec, Point) ->
           fix_diagonal_vector (Line, 0, Vec, Point, Nelts, lists:max (Vec))
       end,
       lists:seq (0, length (Matrix) - 1), Matrix, Points).

outer(Nelts, Points) ->
  {fix_diagonal([ [ distance(A, B) || A <- Points] || B <- Points ], 
                Points, Nelts), 
   [distance({0, 0}, A) || A <- Points]}.

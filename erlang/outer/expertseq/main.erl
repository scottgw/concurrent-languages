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

-module(main).
-export([main/0, main/1]).

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

fix_diagonal(_, [], _, _) -> [];
fix_diagonal(Line, [Head | Tail], [HeadPoints | TailPoints], Nelts) ->
  [ fix_diagonal_vector(Line, 0, Head, HeadPoints, Nelts, lists:max(Head)) |
    fix_diagonal(Line + 1, Tail, TailPoints, Nelts)].

outer(Nelts, Points) ->
  {fix_diagonal(0, [ [ distance(A, B) || A <- Points] || B <- Points ], Points, Nelts), [distance({0, 0}, A) || A <- Points]}.

read_vector_of_points(_,0) -> [];
read_vector_of_points(IsBench,Nelts) -> 
    Val = case IsBench of
              true -> {0, 0};
              false -> 
                  {ok, [X, Y]} = io:fread("", "~d~d"),
                  {X, Y}
          end,
    [ Val | read_vector_of_points(IsBench, Nelts - 1)].

main() -> main(['']).
main(Args) ->
    [Arg|_] = Args,
    IsBench = string:equal (Arg, 'is_bench'),
    {ok, [Nelts]} = io:fread("","~d"),
    Points = read_vector_of_points(IsBench, Nelts),
    case IsBench of
        false -> io:format("~w~n\n", [outer(Nelts, Points)]);
        true -> ''
    end.


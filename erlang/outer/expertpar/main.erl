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

join(Pids) ->
  [receive {Pid, Result} -> Result end || Pid <- Pids].

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

fix_diagonal(Matrix, Points, Nelts) ->
    Parent = self(),
    lists:zipwith3 
      (fun (Line, Vec, Point) -> 
               spawn (fun () -> 
                              Parent ! {self(),
                                        fix_diagonal_vector (Line, 0, Vec, 
                                                             Point, Nelts, 
                                                             lists:max (Vec))}
                      end)
       end,
       lists:seq (0, length (Matrix) - 1), Matrix, Points).

outer(Nelts, Points) ->
  Parent = self(),
  % parallel for on rows
  {join(fix_diagonal(join([spawn(fun() -> Parent ! {self(),
                  [distance(A, B) || A <- Points]} end) || B <- Points]),
      Points, Nelts)),
  [distance({0, 0}, A) || A <- Points]}.


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
    Result = outer(Nelts, Points),
    case IsBench of
        false -> io:format("~w~n\n", [Result]);
        true -> ''
    end.


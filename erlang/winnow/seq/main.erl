%
% winnow: weighted point selection
%
% input:
%   matrix: an integer matrix, whose values are used as masses
%   mask: a boolean matrix showing which points are eligible for
%     consideration
%   nrows, ncols: the number of rows and columns
%   nelts: the number of points to select
%
% output:
%   points: a vector of (x, y) points
%

-module(main).
-export([main/0, main/1]).

get_values_vector(_, _, [], []) -> [];
get_values_vector(Line, Col, [ValuesHead | ValuesTail], [
    MaskHead | MaskTail]) ->
  Rest = get_values_vector(Line, Col + 1, ValuesTail, MaskTail),
  if MaskHead == 1 -> [
        {ValuesHead, {Line, Col}} | Rest];
    true -> Rest
  end.

get_values(_, [], []) -> [];
get_values(Line, [MatrixHead | MatrixTail], [MaskHead | MaskTail]) ->
  get_values_vector(Line, 0, MatrixHead, MaskHead) ++ get_values(
    Line + 1, MatrixTail, MaskTail).

drop(L, 0) -> L;
drop([_ | T], I) -> drop (T, I - 1).

get_points(0, _, _) -> [];
get_points(Nelts, [{_, {I, J}} | Tail], Chunk) ->
  [ {I, J} | get_points(Nelts - 1, drop(Tail, Chunk - 1), Chunk)].

winnow(_, _, Matrix, Mask, Nelts) ->
  Values = get_values(0, Matrix, Mask),
  Sorted = lists:sort(Values),
  N = length(Sorted),
  Chunk = N div Nelts,
  Points = get_points(Nelts, Sorted, Chunk),
  [Points].

read_vector(_, _, 0) -> [];
read_vector(IsBench, Nrows, Ncols) -> 
    Val = 
        if 
          IsBench -> 
            (Nrows * Ncols rem (Ncols + 1)) == 1;
          true -> 
            {ok, [Value]} = io:fread("", "~d"),
            Value
        end,
    [ Val | read_vector(IsBench, Nrows, Ncols - 1)].

read_matrix(_, 0, _) -> [];
read_matrix(IsBench, Nrows, Ncols) -> 
    [read_vector(IsBench, Nrows, Ncols) | read_matrix(IsBench, Nrows - 1, Ncols)].

main() -> main(['']).
main(Args) ->
  [Arg|_] = Args,
  IsBench = string:equal (Arg, 'is_bench'),
  {ok, [Nrows, Ncols]} = io:fread("","~d~d"),
  Matrix = read_matrix(IsBench, Nrows, Ncols),
  Mask = read_matrix(IsBench, Nrows, Ncols),
  {ok, [Nelts]} = io:fread("", "~d"),
  if 
      not IsBench ->
          io:format("~w~n\n", [winnow(Nrows, Ncols, Matrix, Mask, Nelts)]);
      true -> ''
  end. 


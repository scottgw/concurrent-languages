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

-module(winnow).
-export([winnow/5]).

get_values_vector(Line, Col, Values, Mask) ->
    get_values_vector_acc (Line, Col, Values, Mask, []).

get_values_vector_acc(_, _, [], [], Acc) -> lists:reverse (Acc);
get_values_vector_acc(Line, Col, 
                      [ValuesHead | ValuesTail], 
                      [MaskHead | MaskTail], Acc) ->
    case MaskHead of
        true -> 
            get_values_vector_acc (Line, Col+1, ValuesTail, MaskTail,
                                   [{ValuesHead, {Line, Col}} | Acc]);
        _ -> 
            get_values_vector_acc (Line, Col+1, ValuesTail, MaskTail, Acc)
    end.

get_values(Line, Matrix, Mask) ->
    get_values_acc (Line, Matrix, Mask, []).

get_values_acc(_, [], [], Acc) -> lists:flatten (lists:reverse (Acc));
get_values_acc(Line, [MatrixHead | MatrixTail], [MaskHead | MaskTail], Acc) ->
    Rest = get_values_vector(Line, 0, MatrixHead, MaskHead),
    get_values_acc(Line + 1, MatrixTail, MaskTail, [Rest|Acc]).


get_points (Nelts, Ls, Chunk) ->
  get_points_acc (Nelts, Ls, Chunk, []).

get_points_acc (0, _, _, Acc) -> lists:reverse (Acc);
get_points_acc (Nelts, [{_, {I, J}} | Tail], Chunk, Acc) ->
  get_points_acc (Nelts - 1, 
    lists:nthtail (Chunk-1, Tail), 
    Chunk, 
    [{I,J}] ++ Acc).

winnow(_, _, Matrix, Mask, Nelts) ->
  Values = get_values(0, Matrix, Mask),
  Sorted = lists:sort(Values),
  N = length(Sorted),
  Chunk = N div Nelts,
  Points = get_points(Nelts, Sorted, Chunk),
  Points.

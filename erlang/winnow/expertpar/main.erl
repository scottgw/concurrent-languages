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

get_values_vector(Line, Col, Values, Mask) ->
    get_values_vector_acc (Line, Col, Values, Mask, []).

get_values_vector_acc(_, _, [], [], Acc) -> lists:reverse (Acc);
get_values_vector_acc(Line, Col, 
                      [ValuesHead | ValuesTail], 
                      [MaskHead | MaskTail], Acc) ->
    case MaskHead of
        1 -> 
            get_values_vector_acc (Line, Col+1, ValuesTail, MaskTail,
                                   [{ValuesHead, {Line, Col}} | Acc]);
        _ -> 
            get_values_vector_acc (Line, Col+1, ValuesTail, MaskTail, Acc)
    end.

join(Pids) ->
  [receive {Pid, Result} -> Result end || Pid <- Pids].

% worker, processes and sends the results back
get_values_worker(Parent, Line, Col, Values, Mask) ->
  spawn(fun() ->
        Result = get_values_vector(Line, Col, Values, Mask),
        Parent ! {self(), Result}
  end).

get_values_impl(_, _, [], [], Acc) -> lists:reverse (Acc);
get_values_impl(Parent, Line, [MatrixHead | MatrixTail], [MaskHead |
    MaskTail], Acc) ->
  % parallel for on rows
  Worker = get_values_worker (Parent, Line, 0, MatrixHead, MaskHead),
  get_values_impl (Parent, Line + 1, MatrixTail, MaskTail, [Worker|Acc]).

get_values(Line, Matrix, Mask) ->
  Parent = self(),
  Pids = get_values_impl(Parent, Line, Matrix, Mask, []),
  Results = lists:flatten(join(Pids)),
  Results.

get_points(0, _, _) -> [];
get_points(Nelts, [{_, {I, J}} | Tail], Chunk) ->
  [ {I, J} | get_points(Nelts - 1, lists:nthtail(Chunk - 1, Tail), Chunk)].

sort_impl(L) ->
  Parent = self(),
  % parallel for on L
  join([ spawn(fun() -> Parent ! {self(), sort(X)} end) || X <- L ]).

sort([]) -> [];
sort([Pivot | Tail]) ->
  Left = [X || X <- Tail, X < Pivot],
  Right = [X || X <- Tail, X >= Pivot],
  Results = sort_impl([Left, Right]),
  hd(Results) ++ [Pivot] ++ hd(tl(Results)).

winnow(_, _, Matrix, Mask, Nelts) ->
  Values = get_values(0, Matrix, Mask),
  Sorted = sort(Values),
  N = length(Sorted),
  Chunk = N div Nelts,
  Points = get_points(Nelts, Sorted, Chunk),
  [Points].

read_vector(_, _, 0) -> [];
read_vector(IsBench, Nrows, Ncols) -> 
    Val = 
        if 
          IsBench -> 
            ((Nrows * Ncols) rem (Ncols + 1)) == 1;
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

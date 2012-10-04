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

% bounded sort from: http://jinnipark.tumblr.com/post/156214523/erlang-parallel-quick-sort
p_qsort(L) ->
  Self = self(),
  Ref = erlang:make_ref(), % make a unique id
  spawn(fun() ->
    split(L, 1000, Self, Ref)
  end),
  merge(Ref).

split([], _Limit, Parent, Ref) ->
  Parent ! {Ref, []};
split([Pivot | T], Limit, Parent, Ref) when Limit > 2 ->
  Self = self(),
  Limit2 = Limit div 2,
  Ref1 = erlang:make_ref(),
  Ref2 = erlang:make_ref(),
  spawn(fun() ->
    split([X || X <- T, X < Pivot], Limit2, Self, Ref1)
  end),
  spawn(fun() ->
    split([X || X <- T, not (X < Pivot)], Limit2, Self, Ref2)
  end),
  Parent ! {Ref, merge(Ref1) ++ [Pivot] ++ merge(Ref2)};
split(L, _Limit, Parent, Ref) ->
  Parent ! {Ref, lists:sort(L)}.

merge(Ref) ->
  receive
    {Ref, Value} ->
      Value
  end.

winnow(_, _, Matrix, Mask, Nelts) ->
    Values = get_values(0, Matrix, Mask),
    Sorted = p_qsort(Values),
    N = length(Sorted),
    Chunk = N div Nelts,
    Points = get_points(Nelts, Sorted, Chunk),
    Points.

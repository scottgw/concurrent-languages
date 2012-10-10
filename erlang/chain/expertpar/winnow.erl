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

row_process (Parent, {RowNo, Columns, Masks}, Ncols) ->
    ColumnData = lists:zip3( Columns,
                             Masks,
                             lists:seq(0, Ncols - 1) ),
    Weighted =
        [ {W, {RowNo, ColNo}}
          || {W, Mask, ColNo} <- ColumnData, Mask ],
    Parent ! Weighted.

% bounded sort from: http://jinnipark.tumblr.com/post/156214523/erlang-parallel
p_qsort(L) ->
    Self = self(),
    Ref = erlang:make_ref(), % make a unique id
    spawn(fun() ->
                  split(L, 4096, Self, Ref)
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

winnow(Nrows, Ncols, Matrix, Mask, Nelts) ->
    Rows = lists:zip3(lists:seq(0, Nrows-1),
                      Matrix, Mask),
    Parent = self(),
    [ spawn(fun () ->
                    row_process (Parent, Row, Ncols)
            end)
      || Row <- Rows],
    Results = [ receive
                    Res ->
                        Res
                end
                || _ <- Rows],
    FlatRes = lists:flatten (Results),
    Sorted = p_qsort (FlatRes), % lists:sort( FlatRes ),
    Masked = length (FlatRes), % no_masked (Mask),
    io:format("masked ~w~n",[Masked]),
    % ToDrop = max(0, Masked - Nelts),
    % {_, FinalN} = lists:split (ToDrop, Sorted),
    FinalN = drop_them (Masked div Nelts, Nelts, Sorted, 0, []),
    [ Coord || {_, Coord} <- FinalN ].

drop_them (Stride, Len, _, AccLen, Acc) when Len == AccLen -> 
    lists:reverse (Acc);
drop_them (Stride, Len, [Head|Tail], AccLen, Acc) ->
    drop_them (Stride, Len, lists:nthtail(Stride - 1,Tail), 
               AccLen+1, [Head|Acc]).

no_masked(Mask) ->
    lists:sum(lists:map(fun(true) -> 1;
                           (false) -> 0
                        end,
                        lists:flatten(Mask))
             ).

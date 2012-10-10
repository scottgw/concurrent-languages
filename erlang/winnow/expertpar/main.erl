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

row_process (Parent, {RowNo, Columns, Masks}, Ncols) ->
    ColumnData = lists:zip3( Columns,
                             Masks,
                             lists:seq(0, Ncols - 1) ),
    Weighted =
        [ {W, {RowNo, ColNo}}
          || {W, 1, ColNo} <- ColumnData ],
    Parent ! Weighted.
-% bounded sort from: http://jinnipark.tumblr.com/post/156214523/erlang-parallel
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

winnow(Nrows, Ncols, Matrix, Mask, Nelts) ->
    Rows = lists:zip3(lists:seq(0, Nelts-1),
                      Matrix, Mask),
    Parent = self(),
    [ spawn(fun () ->
                    row_process (Parent, Row, Ncols)
            end)
      || Row <- Rows],
    Result = [ receive
                   Res ->
                       Res
               end
               || _ <- Rows],
    Sorted = lists:sort( lists:flatten (Results) ),
    Masked = lists:sum( lists:flatten(Mask) ),
    ToDrop = max(0, Masked - Nelts),
    {_, FinalN} = lists:split (ToDrop, Sorted),
    [[ Coord || {_, Coord} <- FinalN ]].

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

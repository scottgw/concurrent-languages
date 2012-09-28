%
% thresh: histogram thresholding
%
% input:
%   matrix: the integer matrix to be thresholded
%   nrows, ncols: the number of rows and columns
%   percent: the percentage of cells to retain
%
% output:
%   mask: a boolean matrix filled with true for cells that are kept
%

-module(main).
-export([main/0,main/1]).

max(A, B) ->
  if A > B -> A;
    true -> B
  end.

max_matrix(Matrix) ->
  lists:foldl(fun(X, Max) -> max(lists:max(X), Max) end, 0, Matrix).

count_equal(Matrix, Value) ->
  lists:foldl(fun(X, Count) -> Count + length(
          lists:filter(fun(Y) -> Y == Value end, X)) end, 0, Matrix).

fill_histogram (Matrix, Nmax) ->
    lists:map (fun (X) ->
                        count_equal (Matrix, X)
               end, lists:seq (Nmax, 0, -1)).

get_threshold(-1, [], _) -> 0;
get_threshold(Index, [Head | Tail], Count) ->
  if 
      Count - Head < 0 -> Index;
      true -> get_threshold(Index - 1, Tail, Count - Head)
  end.

filter(Matrix, Threshold) ->
  lists:map(fun(X) ->
        lists:map(fun(Y) -> Y >= Threshold end, X) end, Matrix).

thresh(Nrows, Ncols, Matrix, Percent) ->
  Nmax = max_matrix(Matrix),
  Histogram = fill_histogram(Matrix, Nmax),
  Count = (Nrows * Ncols * Percent) / 100,
  Threshold = get_threshold(Nmax, Histogram, Count),
  Mask = filter(Matrix, Threshold),
  Mask.

read_vector(_, _, 0) -> [];
read_vector(IsBench, Nrows, Ncols) -> 
    Val = 
        if 
            IsBench -> Nrows * Ncols rem 100;
            true -> {ok, [Value]} = io:fread("", "~d"),
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
  {ok, [Percent]} = io:fread("", "~d"),
  if 
      not IsBench ->
          io:format("~w~n\n", [thresh(Nrows, Ncols, Matrix, Percent)]);
      true -> ''
  end.


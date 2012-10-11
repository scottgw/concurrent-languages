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

-module(thresh).
-export([thresh/4]).

max(A, B) ->
  if A > B -> A;
    true -> B
  end.

max_matrix(Matrix) ->
  lists:foldl(fun(X, Max) -> max(lists:max(X), Max) end, 0, Matrix).

count_equal(Matrix, Value) ->
  lists:foldl(fun(X, Count) -> Count + length(
          lists:filter(fun(Y) -> Y == Value end, X)) end, 0, Matrix).
empty_hist () ->
    orddict:from_list ([{X,0} || X <- lists:seq (0, 99)]).    

row_hist (Row) ->
    lists:foldl (fun (Elem, HistAcc) ->
                         orddict:update_counter (Elem, 1, HistAcc)
                 end, empty_hist(), Row).

fill_histogram (Matrix, Nmax) ->
    Hists = lists:map (fun (R) -> row_hist (R) end, Matrix),
    Hist = lists:foldl (fun (Hist, HistAcc) ->
                                orddict:merge(fun (K, V1, V2) ->
                                                      V1 + V2
                                              end, Hist, HistAcc)
                        end, empty_hist(), Hists),
    lists:reverse(lists:map (fun ({Idx, Count}) ->
                                     Count
                             end, orddict:to_list (Hist))).

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

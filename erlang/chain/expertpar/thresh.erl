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
-export([row_hist/2]).

% worker, processes and sends the results back
reduce2d_worker(Parent, X, Function) ->
  spawn(fun() ->
      Result = Function(X),
      Parent ! Result
  end).

reduce2d_join(Pids) ->
  [receive 
       Result -> Result 
   end 
   || _Pid <- Pids].

reduce2d(Matrix, Agregator, Function) ->
  Parent = self(),
  % parallel for on rows
  Pids = [reduce2d_worker(Parent, X, Function) || X <- Matrix],
  Agregator(reduce2d_join(Pids)).

max_matrix(Matrix) ->
  reduce2d(Matrix, fun lists:max/1, fun lists:max/1).

empty_hist () ->
    orddict:from_list ([{X,0} || X <- lists:seq (0, 99)]).    

row_hist (Parent, Row) ->
    Hist = lists:foldl (fun (Elem, HistAcc) ->
                                orddict:update_counter (Elem, 1, HistAcc)
                        end, empty_hist(), Row),
    Parent ! Hist.

fill_histogram (Matrix, Nmax) ->
    Parent = self(),
    Pids = [spawn(?MODULE, row_hist, [Parent, Row]) || Row <- Matrix],
    Hists = [receive
                 Result ->
                     Result
             end || _Pid <- Pids], 
    Hist = lists:foldl (fun (Hist, HistAcc) ->
                                orddict:merge(fun (K, V1, V2) ->
                                                      V1 + V2
                                              end, Hist, HistAcc)
                        end, empty_hist(), Hists),
    lists:reverse(lists:map (fun ({Idx, Count}) ->
                       Count
               end, orddict:to_list (Hist))).
    

get_threshold(-1, [], _) -> 0;
get_threshold(Index, [Head | _], Count) when Head > Count ->
    Index;
get_threshold(Index, [Head | Tail], Count) ->
    get_threshold (Index - 1, Tail, Count - Head).

filter(Matrix, Threshold) ->
  reduce2d(Matrix, 
           fun(X) -> X end, 
           fun(X) ->
                   lists:map(fun(Y) -> 
                                     Y >= Threshold 
                             end, 
                             X) 
           end).

thresh(Nrows, Ncols, Matrix, Percent) ->
  Nmax = max_matrix(Matrix),
  Histogram = fill_histogram(Matrix, Nmax),
  Count = (Nrows * Ncols * Percent) / 100,
  Threshold = get_threshold(Nmax, Histogram, Count),
  Mask = filter(Matrix, Threshold),
  Mask.

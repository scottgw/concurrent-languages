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

-module(outer).
-export([outer/2]).

join(Pids) ->
    [receive 
         {Pid, Result} -> 
             Result 
     end 
     || Pid <- Pids].

sqr(X) ->
    X * X.

distance({Ax, Ay}, {Bx, By}) ->
    math:sqrt(sqr(Ax - Bx) + sqr(Ay - By)).

calc_row (Parent, Nelts, {RowN, RowPairs}) ->
    Dist =  [ distance (X,Y) || {X,Y} <- RowPairs ],
    Max = lists:max (Dist),
    {Pre, [_|Post]} = lists:split(RowN - 1, Dist),
    Parent ! {self(), Pre ++ [Max*Nelts|Post]}.

outer(Nelts, Points) ->
    Parent = self(),
    Pairs = [[ {A,B} || A <- Points] || B <- Points ],
    Idxs = lists:seq(1, Nelts),
    RowAndPairs = lists:zip(Idxs, Pairs),
    Pids = [spawn(fun() -> calc_row(Parent, Nelts, Row) end)
            || Row <- RowAndPairs ],
    {join(Pids),
     [distance ({0,0}, A) || A <- Points]}.

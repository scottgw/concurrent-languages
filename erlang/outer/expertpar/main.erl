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

-module(main).
-export([main/0, main/1]).

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
    RowAndPairs = lists:zip(lists:seq(1,Nelts), Pairs),
    Pids = [spawn(fun() -> calc_row(Parent, Nelts, Row) end)
            || Row <- RowAndPairs ],
    {join(Pids),
     [distance ({0,0}, A) || A <- Points]}.

read_vector_of_points(_,0) -> [];
read_vector_of_points(IsBench,Nelts) -> 
    Val = case IsBench of
              true -> {0, 0};
              false -> 
                  {ok, [X, Y]} = io:fread("", "~d~d"),
                  {X, Y}
          end,
    [ Val | read_vector_of_points(IsBench, Nelts - 1)].

main() -> main(['']).
main(Args) ->
    [Arg|_] = Args,
    IsBench = string:equal (Arg, 'is_bench'),
    {ok, [Nelts]} = io:fread("","~d"),
    Points = read_vector_of_points(IsBench, Nelts),
    Result = outer(Nelts, Points),
    case IsBench of
        false -> io:format("~w~n\n", [Result]);
        true -> ''
    end.


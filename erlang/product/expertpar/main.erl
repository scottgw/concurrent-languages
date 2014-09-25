%
% product: a matrix-vector product
%
% input:
%   nelts: the number of elements
%   matrix: a real matrix
%   vector: a real vector
%
% output:
%   result: a real vector, whose values are the result of the product
%

-module(main).
-export([main/0, main/1]).
-export([product/3]).

join(Pids) ->
    Parts = [receive {Pid, Result} -> Result end || Pid <- Pids],
    lists:reverse(lists:append(Parts)).

%% prod_row(Row,Vec,_Acc) ->
%%      lists:sum(lists:zipwith(fun (X,Y) ->
%%                                       X * Y
%%                              end,
%%                              Row,
%%                              Vec)).

prod_row ([], [], Acc) ->
    Acc;
prod_row ([R|Row], [V|Vector], Acc) ->
    prod_row (Row, Vector, Acc + (R*V)).

prod_chunk([], _Vector, Acc) ->
    Acc;
prod_chunk([C|Chunk], Vector, Acc) ->
    X = prod_row(C, Vector, 0),
    prod_chunk(Chunk, Vector, [X|Acc]).

chunk(Matrix, L, Acc) ->
    S = 16,
    if
        L > S ->
            {X, Rest} = lists:split(S, Matrix),
            chunk (Rest, L - S, [X|Acc]);
        true -> [Matrix | Acc]
    end.

%% run(Parent) ->
%%     receive
%%         {Chunk, Vector} ->
%%             Parent ! prod_chunk(Chunk, Vector, [])
%%     end.
    

product(Nelts, Matrix, Vector) ->
    Parent = self(), % parallel for on rows
    Chunked = chunk(Matrix, Nelts, []),
    %% Pids = [{C, spawn(fun() -> run(Parent) end)} || C <- Chunked],
    %% [Pid ! {C, Vector} || {C, Pid} <- Pids],
    
    %% join(Pids).
    {Time, Answer} = 
        timer:tc(
          fun() ->
                  join([spawn(
                          fun() -> 
                                  Parent ! {self(), prod_chunk (C, Vector,[])}
                          end) || C <- Chunked])
          end),
    io:format(standard_error, "~p~n", [Time/1000000]).
read_vector(IsBench, 0) -> [];
read_vector(IsBench, Nelts) -> 
    Val = if IsBench -> 0;
             true -> {ok, [X]} = io:fread("", "~f"), X
          end,
    [ Val | read_vector(IsBench, Nelts - 1)].

read_matrix(IsBench, 0, _) -> [];
read_matrix(IsBench, Nelts, Total) ->
  [ read_vector(IsBench, Total) | read_matrix(IsBench, Nelts - 1, Total)].

run(Nelts) ->
    Matrix = read_matrix(true, Nelts, Nelts),
    Vector = read_vector(true, Nelts),
    %% fprof:trace(start),
    product(Nelts, Matrix, Vector).
    %% fprof:trace(stop),
    %% fprof:profile(),
    %% fprof:analyse(),
    
    

main() -> main(['']).
main(Args) ->
    [Head | _] = Args,
    Nelts = list_to_integer(atom_to_list(Head)),
    run(Nelts).


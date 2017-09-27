%%%-------------------------------------------------------------------
%%% @author clanchun <clanchun@gmail.com>
%%% @copyright (C) 2017, clanchun
%%% @doc Pipe for Erlang
%%%
%%% @end
%%% Created : 26 Sep 2017 by clanchun <clanchun@gmail.com>
%%%-------------------------------------------------------------------
-module(epipe_transform).

-include("epipe.hrl").

-define(PIPE_NAME_CNT, '$pipe_name_cnt').
-define(PIPE_NAME_PREFIX, "_PIPE_TMP_NAME_").

-export([parse_transform/2]).

parse_transform(AST, _Options) ->
    put(?PIPE_NAME_CNT, 0),
    NewAST = lists:reverse(walk_ast(AST, [])),
    erase(?PIPE_NAME_CNT),
    NewAST.

walk_ast([], Acc) ->
    Acc;
walk_ast([{function, _, _, _, _} = Fun | T], Acc) ->
    walk_ast(T, [walk_function(Fun) | Acc]);
walk_ast([H | T], Acc) ->
    walk_ast(T, [H | Acc]).

walk_function({function, Line, Name, Arity, Clauses}) ->
    {function, Line, Name, Arity, walk_clause(Clauses, [])}.

walk_clause([], Acc) ->
    lists:reverse(Acc);
walk_clause([{clause, Line, Args, Gaurds, Body} | T], Acc) ->
    walk_clause(T, [{clause, Line, Args, Gaurds, walk_body(Body, [])} | Acc]);
walk_clause([H | T], Acc) ->
    walk_clause(T, [H | Acc]).

walk_body([], Acc) ->
    lists:reverse(Acc);
walk_body([{match, _, {var, _, Name},
            {cons, _, {atom, _, ?PIPE_MAGIC}, 
             {cons, _, _, _}} = Pipe} | T],
          Acc) ->
    Matches = walk_body([Pipe], []),
    {match, Line, {var, _, LastName}, _} = lists:last(Matches),
    NewLastMatch = {match, Line, {var, Line, Name}, {var, Line, LastName}},
    walk_body(T, lists:reverse(Matches ++ [NewLastMatch]) ++ Acc);
walk_body([{cons, _, {atom, _, ?PIPE_MAGIC},
            {cons, _, _, _} = Pipe} | T],
          Acc) ->
    walk_body(T, replace(Pipe) ++ Acc);
walk_body([H | T], Acc) ->
    walk_body(T, [H | Acc]).

replace(Pipe) ->
    replace(Pipe, 0, undefined, []).

replace({nil, _}, _, _, Acc) ->
    Acc;
replace({cons, Line, H, T}, 0, _, Acc) ->
    FirstName = name(),    
    replace(T, 1, FirstName, [{match, Line, {var, Line, FirstName}, H} | Acc]);
replace({cons, Line, {tuple, _, [Call, {integer, _, Nth}]}, T}, N, Name, Acc) ->
    NextName = name(),
    replace(T, N + 1, NextName, [{match, Line, {var, Line, NextName},
                                  insert_arg(Call, Nth - 1, Name)} | Acc]);
replace({cons, Line, H, T}, N, Name, Acc) ->
    NextName = name(),
    replace(T, N + 1, NextName, [{match, Line, {var, Line, NextName},
                                  insert_arg(H, 0, Name)} | Acc]).
     
name() ->
    N = get(?PIPE_NAME_CNT),
    put(?PIPE_NAME_CNT, N + 1),
    list_to_atom(?PIPE_NAME_PREFIX ++ integer_to_list(N)).

insert_arg({call, Line, Fun, Args}, Nth, Name) ->
    {call, Line, Fun, insert(Args, Nth, {var, Line, Name})};
insert_arg({call, Line, {remote, _, Module, Fun, Args}}, Nth, Name) ->
    {call, Line, Fun, {remote, Line, Module, Fun,
                       insert(Args, Nth, {var, Line, Name})}}.

insert(L, 0, X) ->
    [X | L];
insert([Y | T], N, X) ->
    [Y | insert(T, N - 1, X)].

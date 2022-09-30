%%%-------------------------------------------------------------------
%% @doc three buyers public API
%% @end
%%%-------------------------------------------------------------------

-module(three_buyers_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Tracer = start_tracing(),
    {ok, Supervisor} = three_buyers_sup:start_link(),
    Children = supervisor:which_children(three_buyers_sup),
    {ok, Supervisor, {Tracer, Children}}.

stop({Tracer, Children}) ->
    io:format("== APPLICATION PARTICIPANTS ==~n"),
    io:format("~w~n", [Children]),
    Traces = stop_tracing(Tracer),
    io:format(" == START EXECUTION TRACES (~p) ==~n", [length(Traces)]),
    pretty_print(Traces, Children),
    io:format(" == END EXECUTION TRACES ==~n"),
    ok.

%% internal functions

%% Funcionalidades internas
start_tracing() ->
    Tracer = spawn(fun tracer/0),
    erlang:trace(new_processes, true, [send, 'receive', {tracer, Tracer}]),
    Tracer.

stop_tracing(Tracer) ->
    erlang:trace(all, false, [all]),
    Tracer ! {collect, self()},
    receive
        {Traces, Tracer} ->
            Traces
    end.

tracer() ->
    tracer([]).

tracer(TraceList) ->
    receive
        {collect, From} ->
            From ! {lists:reverse(TraceList), self()};
        Other ->
            tracer([Other | TraceList])
    end.

pretty_print([], _Processes) -> ok;
pretty_print([ T = {trace, Pid, send, _Msg, To} | Rest], Processes) when is_pid(To) ->
    pretty_print_if_interesting(lists:keymember(Pid, 2, Processes) and lists:keymember(To, 2, Processes), T, Processes),
    pretty_print(Rest, Processes);
pretty_print([ T = {trace, Pid, send, _Msg, Ref} | Rest], Processes) when is_atom(Ref) ->
    pretty_print_if_interesting(lists:keymember(Pid, 2, Processes) and lists:keymember(Ref, 1, Processes), T, Processes),
    pretty_print(Rest, Processes);
pretty_print([ T = {trace, Pid, send, _Msg, _Ref} | Rest], Processes) ->
    pretty_print_if_interesting(lists:keymember(Pid, 2, Processes), T, Processes),
    pretty_print(Rest, Processes);
pretty_print([ T = {trace, _SupPid, 'receive', {'EXIT', Pid, normal}} | Rest], Processes) ->
    pretty_print_if_interesting(lists:keymember(Pid, 2, Processes), T, Processes),
    pretty_print(Rest, Processes);
pretty_print([ T = {trace, Pid, 'receive', _Msg} | Rest], Processes) ->
    pretty_print_if_interesting(lists:keymember(Pid, 2, Processes), T, Processes),
    pretty_print(Rest, Processes);
pretty_print([T|Rest], Processes) ->
    io:format("Unexpected trace ~p~n", [T]),
    pretty_print(Rest, Processes).

pretty_print_if_interesting(false, _T, _) -> ok; %io:format("(IGNORED) ~w~n", [T]);
pretty_print_if_interesting(true, {trace, _Pid, 'receive', timeout}, _Processes) -> ok; % ignore timers
pretty_print_if_interesting(true, {trace, _Pid, 'receive', {code_server,_}}, _Processes) -> ok; % ignore the code server
pretty_print_if_interesting(true, {trace, Pid, send, Msg, Ref}, Processes) ->
    {SenderName, Pid, worker,_} = lists:keyfind(Pid, 2, Processes),
    io:format("~p --> ~p : ~w~n", [SenderName, Ref, format(Msg)]);
pretty_print_if_interesting(true, {trace, _SupPid, 'receive', {'EXIT', Pid, normal}}, Processes) ->
    {RecipientName, Pid, worker,_} = lists:keyfind(Pid, 2, Processes),
    io:format("~p ends ~n", [RecipientName]);
pretty_print_if_interesting(true, {trace, Pid, 'receive', Msg}, Processes) ->
    {RecipientName, Pid, worker,_} = lists:keyfind(Pid, 2, Processes),
    io:format("~p processes: ~p~n", [RecipientName, format(Msg)]);
pretty_print_if_interesting(true, T, _P)  ->
    io:format("~w~n", [T]).

format({'$gen_cast',Msg}) -> Msg;
format(Msg) -> Msg.

%%%-------------------------------------------------------------------
%% @doc three buyers seller process
%% @end
%%%-------------------------------------------------------------------
-module(seller).
-behaviour(gen_server).

%% Custom API
-export([start/0]).

%% GenServer API
-export([start_link/1]).
%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start()->'#alice?title<string>.bob!quote<integer>.alice!quote<integer>.&(bob?ok.bob?address<string>.bob!date<string>.end,bob?quit.end)'.
start() ->
    start_link(?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
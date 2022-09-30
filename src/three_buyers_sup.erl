%%%-------------------------------------------------------------------
%% @doc three buyers top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(three_buyers_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    Seller = #{id => seller,
               start => {seller, start, []},
               restart => transient},
    Alice = #{id => alice, 
             start => {alice, start, []},
             restart => transient},
    Bob = #{id => bob, 
             start => {bob, start, []},
             restart => transient},
    Carol = #{id => carol, 
             start => {carol, start, []},
             restart => transient},
    ChildSpecs = [Seller, Alice, Bob, Carol],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
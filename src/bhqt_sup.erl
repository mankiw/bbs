-module(bhqt_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-include("common.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ets:new(ets_info, [set,public,named_table,{keypos,#message.url}]),
    Procs = [
        {pull_server, {pull_server, start_link, []},
            permanent, 5000, worker, [pull_server]},
        {push_server, {push_server, start_link, []},
            permanent, 5000, worker, [push_server]}
    ],
    {ok, { {one_for_one, 5, 10}, Procs} }.


start_child() ->
    ChildSpec =
        {pull_server, {pull_server, start_link, []},  permanent, 5000, worker, [pull_server]},
    supervisor:start_child(?MODULE, ChildSpec).
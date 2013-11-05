-module(bhqt_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    bhqt_sup:start_link().

stop(_State) ->
    ok.

start() ->
    inets:start(),
    application:start(bhqt).


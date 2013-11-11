-module(bhqt_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8085}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    bhqt_sup:start_link().

stop(_State) ->
    ok.

start() ->
    inets:start(),
    application:start(crypto),
    application:start(iconv),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(bhqt).


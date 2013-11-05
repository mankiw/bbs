-module(bhqt_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, get_web/1]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    {ok,Html} = httpc:request("http://bbs.dlut.edu.cn/nforum/article/ITS/152677"),
    Tree = mochiweb_html:parse(term_to_binary(Html)),
    io:format("Tree is ~w", [Tree]),
    bhqt_sup:start_link().

stop(_State) ->
    ok.

start() ->
    inets:start(),
    application:start(hbqt).


get_web(Src) ->ok.

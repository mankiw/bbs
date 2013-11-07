%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{F, Req3} = cowboy_req:qs_val(<<"f">>, Req2),
    {T, Req4} = cowboy_req:qs_val(<<"t">>, Req3),
	{ok, Req5} = echo(Method, Req4, {F,T}),
	{ok, Req5, State}.

echo(<<"GET">>, Req4, {F,T}) ->
    io:format("F is ~w, T is ~w", [F, T]),
    Body = push_server:get_data(binary_to_int(F),binary_to_int(T)),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Body, Req4).

terminate(_Reason, _Req, _State) ->
	ok.

binary_to_int(A) ->
   AS = binary_to_list(A),
   list_to_integer(AS).
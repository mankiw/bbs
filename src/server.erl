%%% -------------------------------------------------------------------
%%% Author  : PXR
%%% Description :
%%%
%%% Created : 2013-11-4
%%% -------------------------------------------------------------------
-module(server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-define(CONST_PAGS_LIST, ["http://bbs.dlut.edu.cn/nforum/board/DUT"]).


%% --------------------------------------------------------------------
%% External exports
-export([update_one/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ets:new(ets_info, set,public,named_table,{keypos,2}),
    erlang:send_after(60*1000, self(), update),
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(update, State) ->
    erlang:send_after(60*1000, self(), update),
    update(now()),
    {onreply, State};
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

update(Time) ->
    HtmlList = ?CONST_PAGS_LIST,
    update(HtmlList, Time).

update([], Time) ->
    ok;
update([Pag|Rest], Time) ->
    update_one(Pag),
    update(Rest, Time).

update_one(Pag) ->
    {ok, Html} = httpc:request(Pag),
    Tree = mochiweb_html:parse(term_to_binary(Html)),
    TitleList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_9']/a/text()", Tree),
    Href = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_9']/a/@href", Tree),
    TimeList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_10']/text()", Tree),
    AuterList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_10']/a/text()", Tree),
    ReplyList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_11 middle']/text()", Tree),
    AuterList2 = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_12']/a/text()", Tree),
    io:format("Href is ~w", [Href]),
    Fun =
        fun(N) ->
            Time = lists:nth(N, TimeList),
            case string:tokens(binary_to_list(Time), [226]) of
                [L,_,_] ->ok;
                [L,_,_,_] ->ok
            end,
            Time2 = lists:nth(N*2-1, AuterList2),
            case string:tokens(binary_to_list(Time2), [226]) of
                [L2] ->ok;
                [L2,_] ->ok;
                [L2,_,_] ->ok;
                [L2,_,_,_] ->ok
            end,
            Auter2 = lists:nth(N*2, AuterList2),
            Auter = lists:nth(N, AuterList),
            Reply = lists:nth(N, ReplyList),
            io:format("Title is ~s ", [lists:nth(N, TitleList)]),
            io:format("Href is ~s", [lists:nth(N, Href)]),
            io:format("Reply is ~s ", [Reply]),
            io:format("Auter is ~s ", [Auter]),
            io:format("Auter2 is ~s ", [Auter2]),
            io:format("Time is ~s~n", [L]),
            io:format("reTime is ~s~n", [L2])
        end,
    lists:foreach(Fun, lists:seq(1, length(TitleList))).
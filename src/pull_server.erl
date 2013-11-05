%%% -------------------------------------------------------------------
%%% Author  : PXR
%%% Description :
%%%
%%% Created : 2013-11-4
%%% -------------------------------------------------------------------
-module(pull_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("common.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([update_one/2, start_link/0,unixtime/0, get_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {list = []}).

%% ====================================================================
%% External functions
%% ====================================================================

get_data(From, To) ->
    SubList = gen_server:call(pull_server, {get_data, From, To}),
    IoFun =
        fun(Msg) ->
                io:format("Title is ~s, time is ~s~n", [Msg#message.title, Msg#message.reply_time_str])
        end,
    lists:foreach(IoFun, SubList);

get_data(From, To) ->
    List = ets:tab2list(ets_info),
    Fun=
        fun(#message{reply_time = T1}, #message{reply_time = T2}) ->
                T1 > T2
        end,
    SortedList = lists:sort(Fun, List),
    SubList = lists:sublist(SortedList, From, To + 1 - From),
    IoFun =
        fun(Msg) ->
                io:format("Title is ~s, time is ~s~n", [Msg#message.title, Msg#message.reply_time_str])
        end,
    lists:foreach(IoFun, SubList).
    
start_link() ->
    gen_server:start_link(?MODULE, [], []).
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
    register(pull_server, self()),
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

handle_call({get_data, F, T}, _From, #state{list = List} = State) ->
    Reply = lists:sublist(List, F, T - F + 1),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
    update(unixtime()),
    List = tidy(),
    push_server:update_date(List),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

update(Time) ->
    HtmlList = ?CONST_PAGS_LIST,
    update(HtmlList, Time).

update([], _Time) ->
    ok;
update([Pag|Rest], Time) ->
    update_one(Pag, Time),
    update(Rest, Time).

update_one(Pag, Now) ->
    {ok, Html} = httpc:request(Pag),
    Tree = mochiweb_html:parse(term_to_binary(Html)),
    TitleList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_9']/a/text()", Tree),
    Href = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_9']/a/@href", Tree),
    TimeList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_10']/text()", Tree),
    AuterList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_10']/a/text()", Tree),
    ReplyList = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_11 middle']/text()", Tree),
    AuterList2 = mochiweb_xpath:execute("//*[@id='body']/div[1]/div[3]/table[2]/*/td[@class = 'title_12']/a/text()", Tree),
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
%%             io:format("Title is ~s ", [lists:nth(N, TitleList)]),
%%             io:format("Href is ~s", [lists:nth(N, Href)]),
%%             io:format("Reply is ~s ", [Reply]),
%%             io:format("Auter is ~s ", [Auter]),
%%             io:format("Auter2 is ~s ", [Auter2]),
%%             io:format("Time is ~s~n", [L]),
%%             io:format("reTime is ~s~n", [L2]),
            case check(L2, Now) of
                false ->
                    ok;
                TimeInit ->
                    Msg = #message{author = Auter,
                                   reply_author = Auter2, 
                                   reply_time_str = L2, 
                                   time_str = L, 
                                   reply_count = Reply,
                                   reply_time = TimeInit,
                                   title = lists:nth(N, TitleList),
                                   url = lists:nth(N, Href)},
                    ets:insert(ets_info, Msg)
            end
        end,
    lists:foreach(Fun, lists:seq(1, length(TitleList))).

tidy() ->
    List = ets:tab2list(ets_info),
    SortFun = 
        fun(#message{reply_time = T1}, #message{reply_time = T2}) ->
                T1 > T2
        end,
    SortList = lists:sort(SortFun, List),
    SubList = lists:sublist(SortList, 1000),
    ets_delete(SortList -- SubList),
    SubList.

ets_delete([]) ->
    ok;
ets_delete([Msg|RestList]) ->
    Key = Msg#message.url,
    ets:delete(ets_info, Key),
    ets_delete(RestList).

check(L2, Time) ->
    TimeList = string:tokens(L2, ":"),
    io:format("Time is ~w, L2 is ~s~n", [Time, L2]),
    case length(TimeList) of
        3 ->
            [H, M, S] = TimeList,
            H1 = list_to_integer(H),
            M1 = list_to_integer(M),
            S1 = list_to_integer(S),
            GTime = calendar:datetime_to_gregorian_seconds({date(),{H1,M1,S1}}),
            Utime = g_2_unix_second(GTime),
            io:format("GTime is ~w~n", [Utime]),
            case Utime - Time < 60 of
                true ->
                    Utime;
                _ ->
                    false
            end;
        _ ->
            false
    end.
            
g_2_unix_second(G) ->
    G_Second = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    G_Second + unixtime() - G.
            
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.
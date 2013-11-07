%%% -------------------------------------------------------------------
%%% Author  : PXR
%%% Description :
%%%
%%% Created : 2013-11-5
%%% -------------------------------------------------------------------
-module(push_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, update_date/1, get_data/2, show_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {list = []}).

%% ====================================================================
%% External functions
%% ====================================================================

get_data(F, T) ->
    SubList = gen_server:call(push_server, {get_data, F, T}),
    FormatFun =
        fun(Msg) ->
                #msg{
                     url = Msg#message.url,
                     title = Msg#message.title,
                     reply_time = list_to_binary(Msg#message.reply_time_str),
                     reply_cout = Msg#message.reply_count,
                     reply_autor = Msg#message.reply_author,
                     autor = Msg#message.author
                     }
        end,
    FormatList = lists:map(FormatFun, SubList),
    msg_pb:encode(FormatList).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

update_date(List) ->
    gen_server:cast(push_server, {update_date, List}).
    
show_data(From, To) ->
    SubList = gen_server:call(push_server, {get_data, From, To}),
    IoFun =
        fun(Msg) ->
                io:format("Title is ~s, time is ~s~n", [Msg#message.title, Msg#message.reply_time_str])
        end,
    lists:foreach(IoFun, SubList).

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
    register(push_server, self()),
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

handle_cast({update_date, List}, State) ->
    {noreply, State#state{list = List}};

handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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


%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2015 3:52 PM
%%%-------------------------------------------------------------------
-module(eldapo_fsm).
-author("sdhillon").

-behaviour(gen_fsm).

%% API
-export([start_link/4]).

-include_lib("LDAP-V3.hrl").

%% gen_fsm callbacks
-export([init/1,
    state_name/2,
    state_name/3,
    pre_ack/2,
    pre_ack/3,
    ready/2,
    ready/3,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {ref, socket, transport, opts, message_counter = 0, backend, backend_state}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
%-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(Ref, Socket, Transport, Opts) ->
    gen_fsm:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, StateName :: atom(), StateData :: #state{}} |
    {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Ref, Socket, Transport, Opts]) ->
    Backend = proplists:get_value(backend, Opts),
    HandlerState = Backend:init(),
    {ok, pre_ack, #state{ref = Ref, socket = Socket, transport = Transport, opts = Opts, backend = Backend, backend_state = HandlerState}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
state_name(_Event, State) ->
    {next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
    {next_state, NextStateName :: atom(), NextState :: #state{}} |
    {next_state, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewState :: #state{}} |
    {stop, Reason :: normal | term(), Reply :: term(),
        NewState :: #state{}}).
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.


pre_ack(timeout, State = #state{ref = Ref, socket = Socket, transport = Transport}) ->
    ok = ranch:accept_ack(Ref),
    Transport:setopts(Socket, [{active, true}]),
    {next_state, ready, State}.

pre_ack(Event, _From, State) ->
    lager:debug("Got event for reply: ~p~n", [Event]),
    Reply = ok,
    {reply, Reply, ready, State}.

ready(Event, State) ->
    lager:debug("Got event: ~p~n", [Event]),
    {next_state, ready, State}.

ready(Event, _From, State) ->
    lager:debug("Got event for reply: ~p~n", [Event]),
    Reply = ok,
    {reply, Reply, ready, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
    {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
    {next_state, NextStateName :: atom(), NewStateData :: #state{},
        timeout() | hibernate} |
    {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
    {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
    {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
    {next_state, NextStateName :: atom(), NewStateData :: term()} |
    {next_state, NextStateName :: atom(), NewStateData :: term(),
        timeout() | hibernate} |
    {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info({tcp_closed, Socket}, _StateName, State = #state{socket = Socket}) ->
    {stop, normal , State};
handle_info({tcp, Socket, Data}, ready, State = #state{socket = Socket}) ->
    process_data(Data, ready, State);

handle_info(Info, StateName, State) ->
    lager:debug("Got Info: ~p in state: ~p, StateName: ~p~n", [Info, State, StateName]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
    {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_data(Data, StateName, State) ->
    {ok, Message} = 'LDAP-V3':decode('LDAPMessage', Data),
    process_message(Message, StateName, State).

process_message(Message, _StateName, State = #state{message_counter = MessageCounter})
        when Message#'LDAPMessage'.messageID < MessageCounter ->
    {stop, wrongMessageCounter, State};
process_message(Message, _StateName, State) when Message#'LDAPMessage'.controls =/= asn1_NOVALUE ->
    {stop, optionNotSupported, State};

process_message(_Message = #'LDAPMessage'{protocolOp = ProtocolOp, messageID = MessageID}, StateName,
        State = #state{backend = Backend, backend_state = BackendState, transport = Transport, socket = Socket})  ->
    %% lager:debug("Got Message: ~p", [ProtocolOp]),
    case Backend:handle(ProtocolOp, BackendState) of
        {stop, none, NewBackendState} ->
            Transport:close(Socket),
            {stop, normal, State#state{message_counter = MessageID + 1, backend_state = NewBackendState }};
        {stop, Reply, NewBackendState} ->
            {ok, ReplyBytes} = 'LDAP-V3':encode('LDAPMessage', #'LDAPMessage'{messageID = MessageID, protocolOp = Reply}),
            Transport:send(Socket, ReplyBytes),
            Transport:close(Socket),
            {stop, normal, State#state{message_counter = MessageID + 1, backend_state = NewBackendState }};
        {error, Error} ->
            lager:error("Error: ~p", [Error]),
            Transport:close(Socket),
            {stop, normal,State#state{message_counter = MessageID + 1}};
        {ok, Reply, NewBackendState} ->
            {ok, ReplyBytes} = 'LDAP-V3':encode('LDAPMessage', #'LDAPMessage'{messageID = MessageID, protocolOp = Reply}),
            lager:debug("Reply Bytes: ~p", [ReplyBytes]),
            lager:debug("Replying with: ~p", [Reply]),
            lager:debug("New Backend State: ~p", [NewBackendState]),
            Transport:send(Socket, ReplyBytes),
            {next_state, StateName, State#state{message_counter = MessageID + 1, backend_state = NewBackendState}}
    end.


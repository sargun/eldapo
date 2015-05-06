%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2015 11:13 PM
%%%-------------------------------------------------------------------
-module(ldap_schema).
-author("sdhillon").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-export([lookup_attribute/1]).

-define(SERVER, ?MODULE).
-include("ldap_schema.hrl").

-record(state, {attribute_types = orddict:new(), object_classes = orddict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

lookup_attribute(Attribute) ->
    case gen_server:call(?SERVER, {lookup_attribute, Attribute}) of
        unknown_attribute ->
            erlang:throw(unknown_attribute);
        Else ->
            Else
    end.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    StartState = #state{},
    State1 = add_attribute_types(StartState),
    State2 = add_object_classes(State1),
    {ok, State2}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({lookup_attribute, Attribute}, _From, State = #state{attribute_types = AttributeTypes}) when is_list(Attribute) ->
    Reply =
        case orddict:find(string:to_lower(Attribute), AttributeTypes) of
            error ->
                unknown;
            {ok, Value} ->
                Value
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_attribute_types(State) ->
    AttributesTypes = [
        #attribute_type{real_name = "objectClass", name = ["objectClass"]},
        #attribute_type{real_name = "commonName", name = ["cn", "commonName"]},
        #attribute_type{real_name = "surname", name = ["sn", "surname"]},
        #attribute_type{real_name = "userPassword", name = ["userPassword"], equality = "octetStringMatch"},
        #attribute_type{real_name = "telephoneNumber", name = ["telephoneNumber"], equality = "telephoneNumberMatch"},
        #attribute_type{real_name = "description", name = ["description"], equality = "caseIgnoreMatch"}
    ],
    NewState = lists:foldl(fun add_attribute_type/2, State, AttributesTypes),
    NewState.

add_attribute_type(AttributeType, State = #state{attribute_types = AttributeTypes}) ->
    Names = AttributeType#attribute_type.name,
    NewAttributeTypes = lists:foldl(fun(Name, Acc) -> orddict:store(string:to_lower(Name), AttributeType, Acc) end, AttributeTypes, Names),
    State#state{attribute_types = NewAttributeTypes}.

add_object_classes(State) ->
    ObjectClasses = [
        #object_class{name = ["person"], must_attributes = ["cn", "sn"],
            may_attributes = ["userPassword", "telephoneNumber", "description"]}
    ],
    NewState = lists:foldl(fun add_object_class/2, State, ObjectClasses),
    NewState.

add_object_class(ObjectClass, State = #state{object_classes = ObjectClasses}) ->
    Names = ObjectClass#object_class.name,
    NewObjectClasses = lists:foldl(fun(Name, Acc) -> orddict:append(Name, ObjectClasses, Acc) end, ObjectClasses, Names),
    State#state{object_classes = NewObjectClasses}.
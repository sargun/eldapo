%%%-------------------------------------------------------------------
%%% @author sdhillon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2015 1:50 PM
%%%-------------------------------------------------------------------
-module(eldapo_backend).
-author("sdhillon").
-include_lib("LDAP-V3.hrl").
-include_lib("riakc/include/riakc.hrl").

%% API
-export([init/0, handle/2]).

-record(state, {bind = anonymous, riak_client}).
-record(bind_object, {name}).
init() ->
    {ok, RiakClient} = riakc_pb_socket:start("127.0.0.1", 8087),
    #state{riak_client = RiakClient}.


handle({bindRequest, Op}, State = #state{riak_client = RiakClient}) ->
    %% Right now, the server just crashes as opposed to coming back with a reasonable error code for any issues
    lager:debug("Handling Bind: ~p with State: ~p", [Op, State]),
    Name = Op#'BindRequest'.name,
    %% TODO: Actually return errors when things are wrong, versus just blow up.
    {simple, Password} = Op#'BindRequest'.authentication,
    3 = Op#'BindRequest'.version,
    BinaryPassword = list_to_binary(Password),
    BinaryName = list_to_binary(Name),
    Query =  <<"userPassword_set:", BinaryPassword/binary, " AND _yz_rk:", BinaryName/binary>>,
    case riakc_pb_socket:search(RiakClient, <<"ldap">>, Query) of
        {ok, SearchResult} when SearchResult#search_results.num_found == 1->
            NewState = State#state{bind = #bind_object{name = Name}},
            Response = {bindResponse,
                #'BindResponse'{
                    resultCode = success,        % result code
                    matchedDN = Name,             % mathedDn, must be string
                    errorMessage = "",             % Error message, must be string
                    referral = asn1_NOVALUE,   % regerral (optional)
                    serverSaslCreds = asn1_NOVALUE    % serverSaslCreds
                }
            };
        Results ->
            lager:debug("Did not get the right response: ~p", [Results]),
            NewState = State,
            Response = {bindResponse,
                #'BindResponse'{
                    resultCode = invalidCredentials,        % result code
                    matchedDN = Name,             % mathedDn, must be string
                    errorMessage = "Invalid Credentials",             % Error message, must be string
                    referral = asn1_NOVALUE,   % regerral (optional)
                    serverSaslCreds = asn1_NOVALUE    % serverSaslCreds
                }
            }
    end,
    {ok, Response, NewState};

%% The entry named in the entry field of the AddRequest MUST NOT exist
%% for the AddRequest to succeed.
%% NOPE. Eventual consistency FTW.

%% The immediate superior (parent) of an
%% object or alias entry to be added MUST exist.
%% NOPE. We can do this later.
handle({addRequest, Op}, State = #state{riak_client = RiakClient}) ->
    lager:debug("Handling add Request: ~p with State: ~p", [Op, State]),

    Bucket = {<<"ldap-objects">>, <<"objects">>},
    Key = list_to_binary(Op#'AddRequest'.entry),
    case riakc_pb_socket:fetch_type(RiakClient, Bucket, Key) of
        {error, {notfound, map}} ->
            Attributes = Op#'AddRequest'.attributes,
            Map = lists:foldl(fun populateAddRequest/2, riakc_map:new(), Attributes),
            lager:debug("Adding Map to Riak: ~p", [Map]),
            riakc_pb_socket:update_type(RiakClient, Bucket, Key, riakc_map:to_op(Map)),
            Response = {addResponse, #'LDAPResult'{resultCode = success, matchedDN = Op#'AddRequest'.entry, errorMessage = "", referral = asn1_NOVALUE}},
            {ok, Response, State};
        {error, _} ->
            Response = {addResponse, #'LDAPResult'{resultCode = unavailable, matchedDN = Op#'AddRequest'.entry, errorMessage = "Object Already Exists", referral = asn1_NOVALUE}},
            {ok, Response, State};
        {ok, _} ->
            Response = {addResponse, #'LDAPResult'{resultCode = entryAlreadyExists, matchedDN = Op#'AddRequest'.entry, errorMessage = "Object Already Exists", referral = asn1_NOVALUE}},
            {ok, Response, State}
    end;
            %{'LDAPResult}

handle({delRequest, DN}, State = #state{riak_client = RiakClient}) ->
    Bucket = {<<"ldap-objects">>, <<"objects">>},
    Key = list_to_binary(DN),
    ok = riakc_pb_socket:delete(RiakClient, Bucket, Key),
    Response = {delResponse, #'LDAPResult'{resultCode = success, matchedDN = DN, errorMessage = "", referral = asn1_NOVALUE}},
    {ok, Response, State};
handle({unbindRequest,'NULL'}, State) ->
    {stop, none, State};
handle(Op, State) ->
    lager:debug("Handling Op: ~p with State: ~p", [Op, State]),
    {stop, none, State}.

%% Set:
populateAddRequest({'AttributeList_SEQOF', AttributeName, AttributeSet}, RiakMap) ->
    AttributeSetBinary = [list_to_binary(Attribute) || Attribute <- AttributeSet],
    riakc_map:update({AttributeName, set},
        fun(Set) ->
            lists:foldl(fun riakc_set:add_element/2, Set, AttributeSetBinary)
        end,
    RiakMap).

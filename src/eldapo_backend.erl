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
-include("ldap_schema.hrl").

%% API
-export([init/0, handle/2]).
-record(bind_object, {name}).

-record(state, {bind = #bind_object{name = anonymous}, riak_client}).
-define(WHOAMI_OID, "1.3.6.1.4.1.4203.1.11.3").
init() ->
    {ok, RiakClient} = riakc_pb_socket:start("127.0.0.1", 8087),
    #state{riak_client = RiakClient}.


handle({bindRequest, Op}, State) ->
    %% Right now, the server just crashes as opposed to coming back with a reasonable error code for any issues
    lager:debug("Handling Bind: ~p with State: ~p", [Op, State]),
    bindRequest(Op, State);

%% The entry named in the entry field of the AddRequest MUST NOT exist
%% for the AddRequest to succeed.
%% NOPE. Eventual consistency FTW.

%% The immediate superior (parent) of an
%% object or alias entry to be added MUST exist.
%% NOPE. We can do this later.
handle({addRequest, Op}, State) ->
    lager:debug("Handling add Request: ~p with State: ~p", [Op, State]),
    addRequest(Op, State);
            %{'LDAPResult}

handle({delRequest, DN}, State) ->
    delRequest(DN, State);
handle({unbindRequest,'NULL'}, State) ->
    {stop, none, State};
handle({modifyRequest, _Op}, State) ->
    {stop, none, State};
%modifyRequest(Op, State);
handle({extendedReq, ExtendedRequest}, State = #state{bind = BindObject})
        when ExtendedRequest == #'ExtendedRequest'{requestName = ?WHOAMI_OID} ->
    Name = case BindObject#bind_object.name of
        anonymous ->
           "";
        Else ->
            Else
    end,
    Response = {extendedResp, #'ExtendedResponse'{resultCode = success, matchedDN = Name, errorMessage = ""}},
    {ok, Response, State};
handle({searchRequest, Op}, State = #state{riak_client = RiakClient}) ->
    lager:debug("Handling Op: ~p with State: ~p", [Op, State]),
    Query = generateQuery(Op),
    lager:debug("Query: ~s", [lists:flatten(Query)]),
    case riakc_pb_socket:search(RiakClient, <<"ldap">>, Query) of
        {ok, Results} ->
            Docs = Results#search_results.docs,
            lager:debug("Query results: ~p", [Docs]),
            BucketKeys = [{{proplists:get_value(<<"_yz_rt">>, KVs), proplists:get_value(<<"_yz_rb">>, KVs)}, proplists:get_value(<<"_yz_rk">>, KVs)} || {_Index, KVs} <- Docs],
            % Because eventual consistency! Between search and KV -- shit sucks
            BucketKeysDedupe = sets:to_list(sets:from_list(BucketKeys)),
            LDAPRecords = [{searchResEntry, key2record(Bucket, Key, RiakClient)} || {Bucket, Key} <- BucketKeysDedupe],
            lager:debug("Records: ~p", [LDAPRecords]),
            Response = LDAPRecords ++ [{'searchResDone', #'LDAPResult'{resultCode = success, matchedDN = "", errorMessage=""}}],
            {ok, Response, State}
    end;
handle(Op, State) ->
    lager:debug("Handling Op: ~p with State: ~p", [Op, State]),
    {stop, none, State}.

key2record({BucketType, Bucket}, CN, RiakClient)
    when BucketType =/= undefined
    andalso Bucket =/= undefined
    andalso CN =/= undefined ->
    {ok, RiakObject} = riakc_pb_socket:fetch_type(RiakClient, {BucketType, Bucket}, CN),
    Attributes = riakc_map:fold(fun attributeToSequence/3, [], RiakObject),
    {ok, StringCN} = asn1rt:utf8_binary_to_list(CN),
    #'SearchResultEntry'{objectName = StringCN, attributes = Attributes}.

attributeToSequence({Key, set}, Items, Acc) ->
    {ok, StringKey} = asn1rt:utf8_binary_to_list(Key),
    StringItems = [Val || {ok, Val} <- [asn1rt:utf8_binary_to_list(Item) || Item <- Items]],
    [#'PartialAttributeList_SEQOF'{type = StringKey, vals = StringItems}|Acc].

check_user_exists({error, Error}) ->
    {error, Error};
check_user_exists({ok, Map}) ->
    case riakc_map:size(Map) of
        0 ->
            {false, Map};
        _ ->
            {true, Map}
    end.

%modifyRequest(Op, State = #state{riak_client = RiakClient}) ->
%    Bucket = {<<"ldap-objects">>, <<"objects">>},
%    Key = list_to_binary(DN#'ModifyRequest'.object),
%    MapResponse = riakc_pb_socket:fetch_type(RiakClient, Bucket, Key),
 %   Response =
  %      case check_user_exists(MapResponse) of
   %         {true, UserMap} ->
    %            NewMap = riakc_map:fold(fun(MapKey, _Val, Acc) -> riakc_map:erase(MapKey, Acc) end, UserMap, UserMap),
%                ok = riakc_pb_socket:update_type(RiakClient, Bucket, Key, riakc_map:to_op(NewMap)),
%                {delResponse, #'LDAPResult'{resultCode = success, matchedDN = DN, errorMessage = "", referral = asn1_NOVALUE}};
%            {false, _UserMap}->
%                {delResponse, #'LDAPResult'{resultCode = noSuchObject, matchedDN = DN, errorMessage = "No Such Object", referral = asn1_NOVALUE}};
%            {error, _} ->
%                {delResponse, #'LDAPResult'{resultCode = unavailable, matchedDN = DN, errorMessage = "", referral = asn1_NOVALUE}}
%        end,
%    {ok, Response, State}.

delRequest(DN, State = #state{riak_client = RiakClient}) ->
    Bucket = {<<"ldap-objects">>, <<"objects">>},
    Key = list_to_binary(DN),
    MapResponse = riakc_pb_socket:fetch_type(RiakClient, Bucket, Key),
    Response =
        case check_user_exists(MapResponse) of
            {true, UserMap} ->
                NewMap = riakc_map:fold(fun(MapKey, _Val, Acc) -> riakc_map:erase(MapKey, Acc) end, UserMap, UserMap),
                ok = riakc_pb_socket:update_type(RiakClient, Bucket, Key, riakc_map:to_op(NewMap)),
                {delResponse, #'LDAPResult'{resultCode = success, matchedDN = DN, errorMessage = "", referral = asn1_NOVALUE}};
            {false, _UserMap}->
                {delResponse, #'LDAPResult'{resultCode = noSuchObject, matchedDN = DN, errorMessage = "No Such Object", referral = asn1_NOVALUE}};
            {error, _} ->
                {delResponse, #'LDAPResult'{resultCode = unavailable, matchedDN = DN, errorMessage = "", referral = asn1_NOVALUE}}
        end,
    {ok, Response, State}.
addRequest(Op, State = #state{riak_client = RiakClient}) ->
    {Bucket, Key} = addRequestToBucketKey(Op),
    MapResponse = riakc_pb_socket:fetch_type(RiakClient, Bucket, Key),
    case check_user_exists(MapResponse) of
        {false, _UserMap} ->
            Attributes = Op#'AddRequest'.attributes,
            Response = case makeAddMap(Attributes) of
                           {ok, Map} ->
                               lager:debug("Adding Map to Riak: ~p", [Map]),
                               ok = riakc_pb_socket:update_type(RiakClient, Bucket, Key, riakc_map:to_op(Map)),
                               {addResponse, #'LDAPResult'{resultCode = success, matchedDN = Op#'AddRequest'.entry, errorMessage = "", referral = asn1_NOVALUE}};
                           {error, unknown_attribute} ->
                               {addResponse, #'LDAPResult'{resultCode = undefinedAttributeType, matchedDN = Op#'AddRequest'.entry, errorMessage = "", referral = asn1_NOVALUE}}
                       end,
            {ok, Response, State};
        {true, _UserMap} ->
            Response = {addResponse, #'LDAPResult'{resultCode = entryAlreadyExists, matchedDN = Op#'AddRequest'.entry, errorMessage = "Object Already Exists", referral = asn1_NOVALUE}},
            {ok, Response, State};
        {error, Error} ->
            lager:error("Failed to get user: ~p", [Error]),
            Response = {addResponse, #'LDAPResult'{resultCode = unavailable, matchedDN = Op#'AddRequest'.entry, errorMessage = "", referral = asn1_NOVALUE}},
            {ok, Response, State}
    end.


makeAddMap(Attributes) ->
    try lists:foldl(fun populateAddRequest/2, riakc_map:new(), Attributes) of
        Map ->
            {ok, Map}
    catch
        error:unknown_attribute ->
            {error, unknown_attribute}
    end.
%% Set:
populateAddRequest(#'AttributeList_SEQOF'{type = AttributeName, vals = AttributeSet}, RiakMap) ->
    AttributeSetBinary = [list_to_binary(Attribute) || Attribute <- AttributeSet],
    RealAttributeName = case ldap_schema:lookup_attribute(AttributeName) of
        unknown ->
            erlang:error(unknown_attribute);
        AttributeType when AttributeType#attribute_type.single_value == false ->
            AttributeType#attribute_type.real_name
    end,
    riakc_map:update({RealAttributeName, set},
        fun(Set) ->
            lists:foldl(fun riakc_set:add_element/2, Set, AttributeSetBinary)
        end,
    RiakMap).

addRequestToBucketKey(AddRequest) when is_record(AddRequest, 'AddRequest') ->
    Bucket = {<<"ldap-objects">>, <<"objects">>},
    Key = list_to_binary(AddRequest#'AddRequest'.entry),
    {Bucket, Key}.

generateQuery(Op) ->
    BaseObject = Op#'SearchRequest'.baseObject,
    BaseQuery =
        case Op#'SearchRequest'.scope of
            baseObject ->
                io_lib:format("(_yz_rk:~s)", [BaseObject]);
            singleLevel ->
                io_lib:format("((_yz_rk:*,~s NOT _yz_rk:*,*,~s) OR _yz_rk:~s)", [BaseObject, BaseObject, BaseObject]);
            wholeSubtree ->
                io_lib:format("(_yz_rk:*,~s OR _yz_rk:~s)", [BaseObject, BaseObject])
        end,
    %% TODO: Add support for (derefencing) aliases.
    neverDerefAliases = Op#'SearchRequest'.derefAliases,
    0 = Op#'SearchRequest'.sizeLimit,
    0 = Op#'SearchRequest'.timeLimit,
    false = Op#'SearchRequest'.typesOnly,
    Filters = generate_filters(Op#'SearchRequest'.filter),
    Query = string:join([BaseQuery, Filters], " AND "),
    Query.

generate_filters({'present', AttributeName}) ->
    Attribute =
        case ldap_schema:lookup_attribute(AttributeName) of
            unknown ->
                erlang:error(unknown_attribute);
            Else ->
                Else
        end,
    Result = case Attribute#attribute_type.single_value of
                 false ->
                     io_lib:format("~s_set:*", [Attribute#attribute_type.real_name])
             end,
    Result;
generate_filters({equalityMatch, {'AttributeValueAssertion', AttributeName, Value}}) ->
    Attribute =
        case ldap_schema:lookup_attribute(AttributeName) of
            unknown ->
                erlang:error(unknown_attribute);
            Else ->
                Else
        end,
    Result = case Attribute#attribute_type.single_value of
       false ->
            io_lib:format("~s_set:~s", [Attribute#attribute_type.real_name, Value])
    end,
    Result;
generate_filters({substrings,{'SubstringFilter', AttributeName, [{MatchType, Value}]}}) ->
    Attribute =
        case ldap_schema:lookup_attribute(AttributeName) of
            unknown ->
                erlang:error(unknown_attribute);
            Else ->
                Else
        end,
    AttributePrefix = case Attribute#attribute_type.single_value of
                 false ->
                     io_lib:format("~s_set:", [Attribute#attribute_type.real_name])
             end,
    Wildcard = case MatchType of
                   initial ->
                       io_lib:format("~s*", [Value]);
                   final ->
                       io_lib:format("*~s", [Value]);
                   any ->
                       io_lib:format("*~s*", [Value])
               end,

    Result = string:concat(AttributePrefix, Wildcard),

    Result;
generate_filters({'and', Filters}) ->
    FilterStrings = [generate_filters(Filter) || Filter <- Filters],
    string:join(FilterStrings, " AND ").

bindRequest(Op, State) when Op#'BindRequest'.name == "" ->
    Response = {bindResponse,
        #'BindResponse'{
            resultCode = success,        % result code
            matchedDN = "",             % mathedDn, must be string
            errorMessage = "",             % Error message, must be string
            referral = asn1_NOVALUE,   % regerral (optional)
            serverSaslCreds = asn1_NOVALUE    % serverSaslCreds
        }
    },
    {ok, Response, State};

bindRequest(Op, State = #state{riak_client = RiakClient}) ->
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
    {ok, Response, NewState}.

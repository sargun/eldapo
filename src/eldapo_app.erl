-module(eldapo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(ldap_handler, 100,
      ranch_tcp, [{packet, asn1}, {port, 6360}], eldapo_fsm, [{backend, eldapo_backend}]),
    eldapo_sup:start_link().

stop(_State) ->
    ok.

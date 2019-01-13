-module(lager_logstash_backend_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT Functions
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Tests
-export([ receives_logs/1
        , respects_level/1
        , init_options/1
        ]).

%%==============================================================================
%% CT functions
%%==============================================================================

all() ->
  [ receives_logs
  , respects_level
  , init_options
  ].

init_per_suite(Config) ->
  application:set_env(lager, error_logger_redirect, false),
  lager:start(),
  Config.

end_per_suite(_) ->
  application:stop(lager).

init_per_testcase(_, Config) ->
  {Socket, Port} = open(),
  start_lager_handler(Port),
  [{socket, Socket}, {port, Port} | Config].

end_per_testcase(_, Config) ->
  stop_lager_handler(),
  gen_udp:close(?config(socket, Config)).

%%==============================================================================
%% Tests
%%==============================================================================

receives_logs(Config) ->
  Socket = ?config(socket, Config),
  LogRef = log(error, "wtf tho?"),
  Logs = flush(Socket),
  assert_received(LogRef, Logs).

respects_level(Config) ->
  Socket = ?config(socket, Config),
  ShouldReceive = log(info, "gogogo"),
  lager:set_loglevel(handler_id(), error),
  ShouldNotReceive = log(info, "noooo"),
  Logs = flush(Socket),
  assert_received(ShouldReceive, Logs),
  assert_not_received(ShouldNotReceive, Logs).

init_options(_) ->
  BadFields = {fields, foobar},
  GoodFields = {fields, [{foo, bar}]},
  GoodHost = {host, <<"123">>},
  GoodPort = {port, 123},

  {error, _} = lager_logstash_backend:init([GoodHost]),
  {error, _} = lager_logstash_backend:init([{port, 0}, GoodHost]),
  {error, _} = lager_logstash_backend:init([GoodPort]),
  {error, _} = lager_logstash_backend:init([GoodPort, GoodHost, BadFields]),
  {ok, _} = lager_logstash_backend:init([GoodPort, GoodHost]),
  {ok, _} = lager_logstash_backend:init([GoodPort, GoodHost, GoodFields]).

%%==============================================================================
%% Private functions
%%==============================================================================

-spec assert_received(Ref | [Ref], [map()]) -> ok when
    Ref :: integer().
assert_received(ExpectedLogRefs, Logs) when is_list(ExpectedLogRefs) ->
  ActualLogRefs = extract_logrefs(Logs),
  lists:foreach( fun (LogRef) ->
                     case sets:is_element(LogRef, ActualLogRefs) of
                       true ->
                         ok;
                       false ->
                         error({ log_not_found
                               , [{log_ref, LogRef}, {logs, Logs}]
                               })
                     end
                 end
               , ExpectedLogRefs);
assert_received(LogRef, Logs) ->
  assert_received([LogRef], Logs).

-spec assert_not_received(Ref | [Ref], [map()]) -> ok when
    Ref :: integer().
assert_not_received(ExpectedLogRefs, Logs) when is_list(ExpectedLogRefs) ->
  ActualLogRefs = extract_logrefs(Logs),
  lists:foreach( fun (LogRef) ->
                     case sets:is_element(LogRef, ActualLogRefs) of
                       false ->
                         ok;
                       true ->
                         error({ forbidding_log_found
                               , [{log_ref, LogRef}, {logs, Logs}]
                               })
                     end
                 end
               , ExpectedLogRefs);
assert_not_received(LogRef, Logs) ->
  assert_not_received([LogRef], Logs).

-spec extract_logrefs([map()]) -> sets:set(integer()).
extract_logrefs(Logs) ->
  Refs = lists:map( fun (#{fields := #{test_log_ref := LogRef}}) -> LogRef end
                  , Logs),
  sets:from_list(Refs).

-spec log(lager:log_level(), string()) -> pos_integer().
log(Level, Message) ->
  LogRef = erlang:unique_integer([positive, monotonic]),
  lager:log(Level, [{test_log_ref, LogRef}], Message),
  LogRef.

-spec start_lager_handler(inet:port_number()) -> ok.
start_lager_handler(Port) ->
  Opts = [{host, host()}, {port, Port}],
  ok = gen_event:add_handler(lager_event, handler_id(), Opts).

-spec stop_lager_handler() -> ok.
stop_lager_handler() ->
  ok = gen_event:delete_handler(lager_event, handler_id(), []).

-spec open() -> {gen_udp:socket(), inet:port_number()}.
open() ->
  {ok, Socket} = gen_udp:open(0, [ binary
                                 , {ip, host()}
                                 , {active, true}
                                 , {reuseaddr, true}
                                 ]),
  {ok, Port} = inet:port(Socket),
  {Socket, Port}.

-spec host() -> inet:ip().
host() ->
  {127, 0, 0, 1}.

-spec handler_id() -> atom().
handler_id() ->
  lager_logstash_backend.

-spec flush(gen_udp:socket()) -> [map()].
flush(Socket) ->
  DecodeOpts = [ return_maps
               , {labels, attempt_atom}
               ],
  [jsx:decode(Packet, DecodeOpts) || Packet <- recv(Socket)].

-spec recv(gen_udp:socket()) -> [binary()].
recv(Socket) ->
  ct:pal("Receiving on port ~p~n", [inet:port(Socket)]),
  recv(Socket, 10, 10, []).

-spec recv(gen_udp:socket(), Tries, timeout(), [binary()]) -> [binary()] when
    Tries :: non_neg_integer().
recv(Socket, Tries, Timeout, Acc0) when Tries > 0 ->
  Acc =
    receive {udp, Socket, _, _, Packet} ->
        [Acc0, Packet]
    after Timeout ->
        Acc0
    end,
  recv(Socket, Tries - 1, Timeout, Acc);
recv(_, 0, _, Acc) ->
  lists:flatten(Acc).

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% End:

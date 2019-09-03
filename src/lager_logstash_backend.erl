-module(lager_logstash_backend).

-behaviour(gen_event).

%% gen_event callbacks
-export([ init/1
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, { level  :: {mask, integer()}
               , host   :: inet:hostname()
               , port   :: inet:port_number()
               , socket :: gen_udp:socket()
               , fields :: [{atom(), jsx:json_term()}]
               }).

-type init_error() :: undefined_host
                    | undefined_port
                    | {bad_fields, term()}
                    | {invalid_port, atom()}
                    | {failed_to_connect, inet:posix()}.

-type config() :: #{ port   := inet:port_number()
                   , host   := inet:hostname()
                   , level  := {mask, integer()}
                   , fields := [{atom(), jsx:json_term()}]
                   }.

%%==============================================================================
%% gen_event callbacks
%%==============================================================================

init(Args) ->
  case parse_opts(Args) of
    {ok, Config} ->
      init_with_config(Config);
    {error, Error} ->
      {error, Error}
  end.

handle_call(get_loglevel, State = #state{level = Level}) ->
  {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
  {ok, ok, State#state{level = lager_util:config_to_mask(Level)}};
handle_call(_Request, State) ->
  {ok, ok, State}.

handle_event({log, Message}, State = #state{level = Level, fields = Fields}) ->
  case lager_util:is_loggable(Message, Level, ?MODULE) of
    true ->
      Data = format_data(Message, Fields),
      #state{port = Port, host = Host, socket = Socket} = State,
      gen_udp:send(Socket, Host, Port, jsx:encode(Data));
    false ->
      ok
  end,
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_info(_, State) ->
  {ok, State}.

terminate(_, #state{socket = Socket}) ->
  gen_udp:close(Socket).

code_change(_, State, _) ->
  {ok, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec format_data(Message, Fields) -> Data when
    Message :: lager_msg:lager_msg(),
    Fields  :: [{atom(), jsx:json_term()}],
    Data    :: jsx:json_term().
format_data(Message, Fields) ->
  Metadata = lager_msg:metadata(Message),
  Defaults = [{severity, lager_msg:severity(Message)}],
  AllFields = Defaults ++ safe_fields(Metadata) ++ Fields,
  #{ fields => AllFields
   , '@timestamp' => format_timestamp(lager_msg:timestamp(Message))
   , message => unicode:characters_to_binary(lager_msg:message(Message))
   }.

-spec safe_fields([{term(), term()}]) -> [{atom() | binary(), jsx:json_term()}].
safe_fields(KVPairs) ->
  lists:map(fun safe_field/1, KVPairs).

-spec safe_field({term(), term()}) -> {atom() | binary(), jsx:json_term()}.
safe_field({Key, Value}) when is_atom(Key);
                              is_binary(Key)->
  {Key, safe_value(Value)};
safe_field({Key, Value}) when is_list(Key) ->
  safe_field({list_to_binary(Key), Value}).

-spec safe_value(term()) -> jsx:json_term().
safe_value(Pid) when is_pid(Pid) ->
  list_to_binary(pid_to_list(Pid));
safe_value(List) when is_list(List) ->
  case io_lib:char_list(List) of
    true ->
      list_to_binary(List);
    false ->
      lists:map(fun safe_value/1, List)
  end;
safe_value(Val) ->
  Val.

-spec format_timestamp(erlang:timestamp()) -> binary().
format_timestamp(Ts = {_, _, Ms}) ->
  {{Y, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(Ts),
  T = [ integer_to_list(Y), $-, i2l(M), $-, i2l(D), $T
      , i2l(H), $:, i2l(Mi), $:, i2l(S), $., i3l(Ms), $Z
      ],
  list_to_binary(T).

-spec i2l(non_neg_integer()) -> string().
i2l(I) when I < 10 ->
  [$0, $0+I];
i2l(I) ->
  integer_to_list(I).

-spec i3l(non_neg_integer()) -> string().
i3l(I) when I < 100 ->
  [$0 | i2l(I)];
i3l(I) ->
  integer_to_list(I).

-spec parse_opts([{atom(), term()}]) -> {ok, config()} | {error, init_error()}.
parse_opts(Opts) ->
  Level = proplists:get_value(level, Opts, info),
  Host = proplists:get_value(host, Opts),
  Port = proplists:get_value(port, Opts),
  Fields = proplists:get_value(fields, Opts, []),
  validate_opts([ {level, Level}
                , {port, Port}
                , {host, Host}
                , {fields, Fields}
                ], #{}).



-spec validate_opts([{atom(), term()}], map()) ->
                       {ok, config()} | {error, init_error()}.
validate_opts([{Key, Value} | Rest], Acc) ->
  case validate_option(Key, Value) of
    ok ->
      validate_opts(Rest, Acc#{Key => Value});
    {ok, NewValue} ->
      validate_opts(Rest, Acc#{Key => NewValue});
    {error, Error} ->
      {error, Error}
  end;
validate_opts([], Acc) ->
  {ok, Acc}.

-spec validate_option(atom(), term()) -> ok | Result when
    Result :: {ok, term()} | {error, init_error()}.
validate_option(level, Level) when is_atom(Level) ->
  {ok, lager_util:config_to_mask(Level)};
validate_option(port, Port) when is_integer(Port),
                                   Port >= 1,
                                   Port =< 65536 ->
  ok;
validate_option(port, undefined) ->
  {error, undefined_port};
validate_option(port, Port) ->
  {error, {invalid_port, Port}};
validate_option(host, undefined) ->
  {error, undefined_host};
validate_option(host, _) ->
  ok;
validate_option(fields, Fields) when is_list(Fields) ->
  ok;
validate_option(fields, Fields) ->
  {error, {bad_fields, Fields}}.

-spec init_with_config(config()) -> {ok, #state{}} | {error, Error} when
    Error :: {error, {failed_to_connect, inet:posix()}}.
init_with_config(#{ level  := Level
                  , host   := Host
                  , port   := Port
                  , fields := Fields
                  }) ->
  case gen_udp:open(0, [binary, {active, false}]) of
    {ok, Socket} ->
      State = #state{ level  = Level
                    , host   = Host
                    , port   = Port
                    , fields = Fields
                    , socket = Socket
                    },
      {ok, State};
    {error, Error} ->
      {error, Error}
  end.

%% Local variables:
%% mode: erlang
%% erlang-indent-level: 2
%% indent-tabs-mode: nil
%% fill-column: 80
%% End:

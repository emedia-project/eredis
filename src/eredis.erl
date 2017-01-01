% @doc
%
% Erlang Redis client
%
% Usage:
% <pre>
% {ok, Client} = eredis:start_link().
% {ok, &lt;&lt;"OK"&gt;&gt;} = eredis:q(Client, ["SET", "foo", "bar"]).
% {ok, &lt;&lt;"bar"&gt;&gt;} = eredis:q(Client, ["GET", "foo"]).
% </pre>
% @end

-module(eredis).
-include("eredis.hrl").

%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

-export([start_link/0, start_link/1, start_link/2, start_link/3, start_link/4,
         start_link/5, start_link/6, stop/1, q/2, q/3, qp/2, qp/3, q_noreply/2]).

%% Exported for testing
-export([create_multibulk/1]).

-export_type([reconnect_sleep/0]).

-type client() :: pid()
                  | atom()
                  | {atom(), atom()}
                  | {global, term()}
                  | {via, atom(), term()}.

-type option() :: {host, string()}
                  | {port, integer()}
                  | {database, string()}
                  | {password, string()}
                  | {reconnect_sleep, reconnect_sleep()}
                  | {connect_timeout, integer()}.
-type server_args() :: [option()].
-type reconnect_sleep() :: no_reconnect | integer().
-type return_value() :: undefined | binary() | [binary() | nonempty_list()].
-type pipeline() :: [iolist()].

% @equiv start_link("127.0.0.1", 6379, 0, "")
-spec start_link() -> any().
start_link() ->
  start_link("127.0.0.1", 6379, 0, "").

% @equiv start_link(Host, Port, 0, "")
-spec start_link(Host :: list(),
                 Port :: integer()) -> {ok, Pid :: pid()}
                                       | {error, Reason :: term()}.
start_link(Host, Port) ->
  start_link(Host, Port, 0, "").

% @equiv start_link(Host, Port, Database, "")
-spec start_link(Host :: list(),
                 Port :: integer(),
                 Database :: integer() | undefined) -> {ok, Pid :: pid()}
                                                       | {error, Reason :: term()}.
start_link(Host, Port, Database) ->
  start_link(Host, Port, Database, "").

% @equiv start_link(Host, Port, Database, Password, 100)
-spec start_link(Host :: list(),
                 Port :: integer(),
                 Database :: integer() | undefined,
                 Password :: list()) -> {ok, Pid :: pid()}
                                        | {error, Reason :: term()}.
start_link(Host, Port, Database, Password) ->
  start_link(Host, Port, Database, Password, 100).

% @equiv start_link(Host, Port, Database, Password, ReconnectSleep, 5000)
-spec start_link(Host :: list(),
                 Port :: integer(),
                 Database :: integer() | undefined,
                 Password :: list(),
                 ReconnectSleep :: integer() | no_reconnect) -> {ok, Pid :: pid()}
                                                                | {error, Reason :: term()}.
start_link(Host, Port, Database, Password, ReconnectSleep) ->
  start_link(Host, Port, Database, Password, ReconnectSleep, ?TIMEOUT).

% @doc
% Start a Redis client
% @end
-spec start_link(Host :: list(),
                 Port :: integer(),
                 Database :: integer() | undefined,
                 Password :: list(),
                 ReconnectSleep :: integer() | no_reconnect,
                 ConnectTimeout :: integer()) -> {ok, Pid :: pid()}
                                                 | {error, Reason :: term()}.
start_link(Host, Port, Database, Password, ReconnectSleep, ConnectTimeout)
  when is_list(Host),
       is_integer(Port),
       is_integer(Database) orelse Database == undefined,
       is_list(Password),
       is_integer(ReconnectSleep) orelse ReconnectSleep =:= no_reconnect,
       is_integer(ConnectTimeout) ->
  eredis_client:start_link(Host, Port, Database, Password,
                           ReconnectSleep, ConnectTimeout).

% @doc
% Start a Redis client
% @end
-spec start_link(server_args()) -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link(Args) ->
  Host           = proplists:get_value(host, Args, "127.0.0.1"),
  Port           = proplists:get_value(port, Args, 6379),
  Database       = proplists:get_value(database, Args, 0),
  Password       = proplists:get_value(password, Args, ""),
  ReconnectSleep = proplists:get_value(reconnect_sleep, Args, 100),
  ConnectTimeout = proplists:get_value(connect_timeout, Args, ?TIMEOUT),
  start_link(Host, Port, Database, Password, ReconnectSleep, ConnectTimeout).

% @doc
% Stop the Redis client.
% @end
-spec stop(Client :: pid()) -> ok.
stop(Client) ->
  eredis_client:stop(Client).

% @equiv q(Client, Command, 5000)
-spec q(Client :: client(), Command :: [any()]) ->
  {ok, return_value()}
  | {error, Reason :: binary() | no_connection}.
q(Client, Command) ->
  call(Client, Command, ?TIMEOUT).

% @doc
% Executes the given command in the specified connection. The
% command must be a valid Redis command and may contain arbitrary
% data which will be converted to binaries. The returned values will
% always be binaries.
% @end
-spec q(Client :: client(), Command :: [any()], Timeout :: integer()) ->
  {ok, return_value()}
  | {error, Reason :: binary() | no_connection}.
q(Client, Command, Timeout) ->
  call(Client, Command, Timeout).

% @equiv qp(Client, Pipeline, 5000)
-spec qp(Client :: client(), Pipeline :: pipeline()) ->
  [{ok, return_value()} | {error, Reason :: binary()}] |
  {error, no_connection}.
qp(Client, Pipeline) ->
  pipeline(Client, Pipeline, ?TIMEOUT).

% @doc
% Executes the given pipeline (list of commands) in the
% specified connection. The commands must be valid Redis commands and
% may contain arbitrary data which will be converted to binaries. The
% values returned by each command in the pipeline are returned in a list.
% @end
-spec qp(Client :: client(), Pipeline :: pipeline(), Timeout :: integer()) ->
  [{ok, return_value()} | {error, Reason :: binary()}]
  | {error, no_connection}.
qp(Client, Pipeline, Timeout) ->
  pipeline(Client, Pipeline, Timeout).

%% @doc Executes the command but does not wait for a response and ignores any errors.
%% @see q/2
-spec q_noreply(Client :: client(), Command :: [any()]) -> ok.
q_noreply(Client, Command) ->
  cast(Client, Command).

% @hidden
call(Client, Command, Timeout) ->
  Request = {request, create_multibulk(Command)},
  gen_server:call(Client, Request, Timeout).

% @hidden
pipeline(_Client, [], _Timeout) ->
  [];
pipeline(Client, Pipeline, Timeout) ->
  Request = {pipeline, [create_multibulk(Command) || Command <- Pipeline]},
  gen_server:call(Client, Request, Timeout).

% @hidden
cast(Client, Command) ->
  Request = {request, create_multibulk(Command)},
  gen_server:cast(Client, Request).

% @hidden
% Creates a multibulk command with all the correct size headers
-spec create_multibulk(Args :: [any()]) -> Command :: iolist().
create_multibulk(Args) ->
  ArgCount = [<<$*>>, integer_to_list(length(Args)), <<?NL>>],
  ArgsBin = lists:map(fun to_bulk/1, lists:map(fun to_binary/1, Args)),
  [ArgCount, ArgsBin].

% @hidden
to_bulk(B) when is_binary(B) ->
  [<<$$>>, integer_to_list(iolist_size(B)), <<?NL>>, B, <<?NL>>].

% @hidden
% Convert given value to binary. Fallbacks to
% term_to_binary/1. For floats, throws {cannot_store_floats, Float}
% as we do not want floats to be stored in Redis. Your future self
% will thank you for this.
to_binary(X) when is_list(X)    -> list_to_binary(X);
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_binary(X)  -> X;
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_float(X)   -> throw({cannot_store_floats, X});
to_binary(X)                    -> term_to_binary(X).


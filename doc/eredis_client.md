

# Module eredis_client #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#do_sync_command-2">do_sync_command/2</a></td><td> Executes the given command synchronously, expects Redis to
return "+OK\r\n", otherwise it will fail.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#select_database-2">select_database/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-6">start_link/6</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="do_sync_command-2"></a>

### do_sync_command/2 ###

`do_sync_command(Socket, Command) -> any()`

Executes the given command synchronously, expects Redis to
return "+OK\r\n", otherwise it will fail.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="select_database-2"></a>

### select_database/2 ###

`select_database(Socket, Database) -> any()`

<a name="start_link-6"></a>

### start_link/6 ###

<pre><code>
start_link(Host::list(), Port::integer(), Database::integer() | undefined, Password::string(), ReconnectSleep::<a href="eredis.md#type-reconnect_sleep">eredis:reconnect_sleep()</a>, ConnectTimeout::integer() | undefined) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

`stop(Pid) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


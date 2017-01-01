

# Module eredis #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

<a name="description"></a>

## Description ##

Erlang Redis client

Usage:

```

 {ok, Client} = eredis:start_link().
 {ok, <<"OK">>} = eredis:q(Client, ["SET", "foo", "bar"]).
 {ok, <<"bar">>} = eredis:q(Client, ["GET", "foo"]).
```

<a name="types"></a>

## Data Types ##




### <a name="type-client">client()</a> ###


<pre><code>
client() = pid() | atom() | {atom(), atom()} | {global, term()} | {via, atom(), term()}
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {host, string()} | {port, integer()} | {database, string()} | {password, string()} | {reconnect_sleep, <a href="#type-reconnect_sleep">reconnect_sleep()</a>} | {connect_timeout, integer()}
</code></pre>




### <a name="type-pipeline">pipeline()</a> ###


<pre><code>
pipeline() = [iolist()]
</code></pre>




### <a name="type-reconnect_sleep">reconnect_sleep()</a> ###


<pre><code>
reconnect_sleep() = no_reconnect | integer()
</code></pre>




### <a name="type-return_value">return_value()</a> ###


<pre><code>
return_value() = undefined | binary() | [binary() | nonempty_list()]
</code></pre>




### <a name="type-server_args">server_args()</a> ###


<pre><code>
server_args() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#q-2">q/2</a></td><td>Equivalent to <a href="#q-3"><tt>q(Client, Command, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#q-3">q/3</a></td><td>
Executes the given command in the specified connection.</td></tr><tr><td valign="top"><a href="#q_noreply-2">q_noreply/2</a></td><td>Executes the command but does not wait for a response and ignores any errors.</td></tr><tr><td valign="top"><a href="#qp-2">qp/2</a></td><td>Equivalent to <a href="#qp-3"><tt>qp(Client, Pipeline, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#qp-3">qp/3</a></td><td>
Executes the given pipeline (list of commands) in the
specified connection.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Equivalent to <a href="#start_link-4"><tt>start_link("127.0.0.1", 6379, 0, "")</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Start a Redis client.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td>Equivalent to <a href="#start_link-4"><tt>start_link(Host, Port, 0, "")</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>Equivalent to <a href="#start_link-4"><tt>start_link(Host, Port, Database, "")</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td>Equivalent to <a href="#start_link-5"><tt>start_link(Host, Port, Database, Password, 100)</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-5">start_link/5</a></td><td>Equivalent to <a href="#start_link-6"><tt>start_link(Host, Port, Database, Password,
ReconnectSleep, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-6">start_link/6</a></td><td>
Start a Redis client.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Stop the Redis client.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="q-2"></a>

### q/2 ###

<pre><code>
q(Client::<a href="#type-client">client()</a>, Command::[any()]) -&gt; {ok, <a href="#type-return_value">return_value()</a>} | {error, Reason::binary() | no_connection}
</code></pre>
<br />

Equivalent to [`q(Client, Command, 5000)`](#q-3).

<a name="q-3"></a>

### q/3 ###

<pre><code>
q(Client::<a href="#type-client">client()</a>, Command::[any()], Timeout::integer()) -&gt; {ok, <a href="#type-return_value">return_value()</a>} | {error, Reason::binary() | no_connection}
</code></pre>
<br />

Executes the given command in the specified connection. The
command must be a valid Redis command and may contain arbitrary
data which will be converted to binaries. The returned values will
always be binaries.

<a name="q_noreply-2"></a>

### q_noreply/2 ###

<pre><code>
q_noreply(Client::<a href="#type-client">client()</a>, Command::[any()]) -&gt; ok
</code></pre>
<br />

Executes the command but does not wait for a response and ignores any errors.

__See also:__ [q/2](#q-2).

<a name="qp-2"></a>

### qp/2 ###

<pre><code>
qp(Client::<a href="#type-client">client()</a>, Pipeline::<a href="#type-pipeline">pipeline()</a>) -&gt; [{ok, <a href="#type-return_value">return_value()</a>} | {error, Reason::binary()}] | {error, no_connection}
</code></pre>
<br />

Equivalent to [`qp(Client, Pipeline, 5000)`](#qp-3).

<a name="qp-3"></a>

### qp/3 ###

<pre><code>
qp(Client::<a href="#type-client">client()</a>, Pipeline::<a href="#type-pipeline">pipeline()</a>, Timeout::integer()) -&gt; [{ok, <a href="#type-return_value">return_value()</a>} | {error, Reason::binary()}] | {error, no_connection}
</code></pre>
<br />

Executes the given pipeline (list of commands) in the
specified connection. The commands must be valid Redis commands and
may contain arbitrary data which will be converted to binaries. The
values returned by each command in the pipeline are returned in a list.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; any()
</code></pre>
<br />

Equivalent to [`start_link("127.0.0.1", 6379, 0, "")`](#start_link-4).

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Args::<a href="#type-server_args">server_args()</a>) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Start a Redis client

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Host::list(), Port::integer()) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_link(Host, Port, 0, "")`](#start_link-4).

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Host::list(), Port::integer(), Database::integer() | undefined) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_link(Host, Port, Database, "")`](#start_link-4).

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Host::list(), Port::integer(), Database::integer() | undefined, Password::list()) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_link(Host, Port, Database, Password, 100)`](#start_link-5).

<a name="start_link-5"></a>

### start_link/5 ###

<pre><code>
start_link(Host::list(), Port::integer(), Database::integer() | undefined, Password::list(), ReconnectSleep::integer() | no_reconnect) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_link(Host, Port, Database, Password,ReconnectSleep, 5000)`](#start_link-6).

<a name="start_link-6"></a>

### start_link/6 ###

<pre><code>
start_link(Host::list(), Port::integer(), Database::integer() | undefined, Password::list(), ReconnectSleep::integer() | no_reconnect, ConnectTimeout::integer()) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Start a Redis client

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Client::pid()) -&gt; ok
</code></pre>
<br />

Stop the Redis client.


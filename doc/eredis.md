

# Module eredis #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-client">client()</a> ###


<pre><code>
client() = pid() | atom() | {atom(), atom()} | {global, term()} | {via, atom(), term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_multibulk-1">create_multibulk/1</a></td><td> Creates a multibulk command with all the correct size headers.</td></tr><tr><td valign="top"><a href="#q-2">q/2</a></td><td> Executes the given command in the specified connection.</td></tr><tr><td valign="top"><a href="#q-3">q/3</a></td><td></td></tr><tr><td valign="top"><a href="#q_noreply-2">q_noreply/2</a></td><td>Executes the command but does not wait for a response and ignores any errors.</td></tr><tr><td valign="top"><a href="#qp-2">qp/2</a></td><td> Executes the given pipeline (list of commands) in the
specified connection.</td></tr><tr><td valign="top"><a href="#qp-3">qp/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td> Callback for starting from poolboy.</td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-5">start_link/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-6">start_link/6</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_multibulk-1"></a>

### create_multibulk/1 ###

<pre><code>
create_multibulk(Args::[any()]) -&gt; Command::iolist()
</code></pre>
<br />

Creates a multibulk command with all the correct size headers

<a name="q-2"></a>

### q/2 ###

<pre><code>
q(Client::<a href="#type-client">client()</a>, Command::[any()]) -&gt; {ok, <a href="#type-return_value">return_value()</a>} | {error, Reason::binary() | no_connection}
</code></pre>
<br />

Executes the given command in the specified connection. The
command must be a valid Redis command and may contain arbitrary
data which will be converted to binaries. The returned values will
always be binaries.

<a name="q-3"></a>

### q/3 ###

`q(Client, Command, Timeout) -> any()`

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

Executes the given pipeline (list of commands) in the
specified connection. The commands must be valid Redis commands and
may contain arbitrary data which will be converted to binaries. The
values returned by each command in the pipeline are returned in a list.

<a name="qp-3"></a>

### qp/3 ###

`qp(Client, Pipeline, Timeout) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Args::<a href="#type-server_args">server_args()</a>) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Callback for starting from poolboy

<a name="start_link-2"></a>

### start_link/2 ###

`start_link(Host, Port) -> any()`

<a name="start_link-3"></a>

### start_link/3 ###

`start_link(Host, Port, Database) -> any()`

<a name="start_link-4"></a>

### start_link/4 ###

`start_link(Host, Port, Database, Password) -> any()`

<a name="start_link-5"></a>

### start_link/5 ###

`start_link(Host, Port, Database, Password, ReconnectSleep) -> any()`

<a name="start_link-6"></a>

### start_link/6 ###

`start_link(Host, Port, Database, Password, ReconnectSleep, ConnectTimeout) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Client) -> any()`


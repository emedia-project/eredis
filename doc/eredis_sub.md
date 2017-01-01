

# Module eredis_sub #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Erlang PubSub Redis client.

<a name="types"></a>

## Data Types ##




### <a name="type-channel">channel()</a> ###


<pre><code>
channel() = binary()
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {host, string()} | {port, integer()} | {database, string()} | {password, string()} | {reconnect_sleep, <a href="eredis.md#type-reconnect_sleep">eredis:reconnect_sleep()</a>} | {max_queue_size, integer() | infinity} | {queue_behaviour, drop | exit}
</code></pre>




### <a name="type-server_args">server_args()</a> ###


<pre><code>
server_args() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ack_message-1">ack_message/1</a></td><td>
Acknowledge the receipt of a pubsub message.</td></tr><tr><td valign="top"><a href="#channels-1">channels/1</a></td><td>
Returns the channels the given client is currently
subscribing to.</td></tr><tr><td valign="top"><a href="#controlling_process-1">controlling_process/1</a></td><td>Equivalent to <a href="#controlling_process-2"><tt>controlling_process(Client, self())</tt></a>.</td></tr><tr><td valign="top"><a href="#controlling_process-2">controlling_process/2</a></td><td>Equivalent to <a href="#controlling_process-3"><tt>controlling_process(Client, Pid, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#controlling_process-3">controlling_process/3</a></td><td>.</td></tr><tr><td valign="top"><a href="#psubscribe-2">psubscribe/2</a></td><td>
Pattern subscribe to the given channels.</td></tr><tr><td valign="top"><a href="#punsubscribe-2">punsubscribe/2</a></td><td>
Pattern unsubscribe to the given channels.</td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Equivalent to <a href="#start_link-1"><tt>start_link([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Start a PubSub Redis client.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td>Equivalent to <a href="#start_link-6"><tt>start_link(Host, Port, Password, 100, infinity, drop)</tt></a>.</td></tr><tr><td valign="top"><a href="#start_link-6">start_link/6</a></td><td>
Start a PubSub Redis client.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Stop the PubSub Redis client.</td></tr><tr><td valign="top"><a href="#subscribe-2">subscribe/2</a></td><td>
Subscribe to the given channels.</td></tr><tr><td valign="top"><a href="#unsubscribe-2">unsubscribe/2</a></td><td>
Unsubscribe to the given channels.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ack_message-1"></a>

### ack_message/1 ###

<pre><code>
ack_message(Client::pid()) -&gt; ok
</code></pre>
<br />

Acknowledge the receipt of a pubsub message. each pubsub
message must be acknowledged before the next one is received

<a name="channels-1"></a>

### channels/1 ###

<pre><code>
channels(Client::pid()) -&gt; [<a href="#type-channel">channel()</a>]
</code></pre>
<br />

Returns the channels the given client is currently
subscribing to. Note: this list is based on the channels at startup
and any channel added during runtime. It might not immediately
reflect the channels Redis thinks the client is subscribed to.

<a name="controlling_process-1"></a>

### controlling_process/1 ###

<pre><code>
controlling_process(Client::pid()) -&gt; ok
</code></pre>
<br />

Equivalent to [`controlling_process(Client, self())`](#controlling_process-2).

<a name="controlling_process-2"></a>

### controlling_process/2 ###

<pre><code>
controlling_process(Client::pid(), Pid::pid()) -&gt; ok
</code></pre>
<br />

Equivalent to [`controlling_process(Client, Pid, 5000)`](#controlling_process-3).

<a name="controlling_process-3"></a>

### controlling_process/3 ###

<pre><code>
controlling_process(Client::pid(), Pid::pid(), Timeout::integer()) -&gt; ok
</code></pre>
<br />

Make the given process (pid) the controlling process subscriber
with the given Timeout.

Thecontrolling process received pubsub-related messages, of which
there are three kinds. In each message, the pid refers to the
eredis client process.

* `{message, Channel :: binary(), Message :: binary(), pid()}` :
This is sent for each pubsub message received by the client.

* `{pmessage, Pattern :: binary(), Channel :: binary(), Message :: binary(), pid()}` :
This is sent for each pattern pubsub message received by the client.

* `{dropped, NumMessages :: integer(), pid()}` :
If the queue reaches the max size as specified in start_link
and the behaviour is to drop messages, this message is sent when
the queue is flushed.

* `{subscribed, Channel :: binary(), pid()}` :
When using eredis_sub:subscribe(pid()), this message will be
sent for each channel Redis aknowledges the subscription. The
opposite, 'unsubscribed' is sent when Redis aknowledges removal
of a subscription.

* `{eredis_disconnected, pid()}` :
This is sent when the eredis client is disconnected from redis.

* `{eredis_connected, pid()}` :
This is sent when the eredis client reconnects to redis after
an existing connection was disconnected.


Any message of the form `{message, _, _, _}` must be acknowledged
before any subsequent message of the same form is sent. This
prevents the controlling process from being overrun with redis
pubsub messages. @see ack_message/1.

<a name="psubscribe-2"></a>

### psubscribe/2 ###

<pre><code>
psubscribe(Client::pid(), Channels::[<a href="#type-channel">channel()</a>]) -&gt; ok
</code></pre>
<br />

Pattern subscribe to the given channels. Returns immediately. The
result will be delivered to the controlling process as any other
message. Delivers {subscribed, Channel :: binary(), pid()}

<a name="punsubscribe-2"></a>

### punsubscribe/2 ###

<pre><code>
punsubscribe(Client::pid(), Channels::[<a href="#type-channel">channel()</a>]) -&gt; ok
</code></pre>
<br />

Pattern unsubscribe to the given channels. Returns immediately.

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_link([])`](#start_link-1).

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Args::<a href="#type-server_args">server_args()</a>) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Start a PubSub Redis client

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(Host::list(), Port::integer(), Password::list()) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_link(Host, Port, Password, 100, infinity, drop)`](#start_link-6).

<a name="start_link-6"></a>

### start_link/6 ###

<pre><code>
start_link(Host::list(), Port::integer(), Password::list(), ReconnectSleep::integer() | no_reconnect, MaxQueueSize::integer() | infinity, QueueBehaviour::drop | exit) -&gt; {ok, Pid::pid()} | {error, Reason::term()}
</code></pre>
<br />

Start a PubSub Redis client

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Client::pid()) -&gt; ok
</code></pre>
<br />

Stop the PubSub Redis client

<a name="subscribe-2"></a>

### subscribe/2 ###

<pre><code>
subscribe(Client::pid(), Channels::[<a href="#type-channel">channel()</a>]) -&gt; ok
</code></pre>
<br />

Subscribe to the given channels. Returns immediately. The
result will be delivered to the controlling process as any other
message. Delivers `{subscribed, Channel :: binary(), pid()}`

<a name="unsubscribe-2"></a>

### unsubscribe/2 ###

<pre><code>
unsubscribe(Client::pid(), Channels::[<a href="#type-channel">channel()</a>]) -&gt; ok
</code></pre>
<br />

Unsubscribe to the given channels. Returns immediately.


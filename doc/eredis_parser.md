

# Module eredis_parser #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-0">init/0</a></td><td> Initialize the parser.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td> Parses the (possibly partial) response from Redis.</td></tr><tr><td valign="top"><a href="#parse_bulk-1">parse_bulk/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_bulk-2">parse_bulk/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_multibulk-1">parse_multibulk/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_multibulk-2">parse_multibulk/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

Initialize the parser

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(State::#pstate{}, Data::binary()) -&gt; {ok, <a href="#type-return_value">return_value()</a>, NewState::#pstate{}} | {ok, <a href="#type-return_value">return_value()</a>, Rest::binary(), NewState::#pstate{}} | {error, ErrString::binary(), NewState::#pstate{}} | {error, ErrString::binary(), Rest::binary(), NewState::#pstate{}} | {continue, NewState::#pstate{}}
</code></pre>
<br />

Parses the (possibly partial) response from Redis. Returns
either {ok, Value, NewState}, {ok, Value, Rest, NewState} or
{continue, NewState}. External entry point for parsing.

In case {ok, Value, NewState} is returned, Value contains the value
returned by Redis. NewState will be an empty parser state.

In case {ok, Value, Rest, NewState} is returned, Value contains the
most recent value returned by Redis, while Rest contains any extra
data that was given, but was not part of the same response. In this
case you should immeditely call parse again with Rest as the Data
argument and NewState as the State argument.

In case {continue, NewState} is returned, more data is needed
before a complete value can be returned. As soon as you have more
data, call parse again with NewState as the State argument and any
new binary data as the Data argument.

<a name="parse_bulk-1"></a>

### parse_bulk/1 ###

`parse_bulk(Data) -> any()`

<a name="parse_bulk-2"></a>

### parse_bulk/2 ###

`parse_bulk(X1, NewData0) -> any()`

<a name="parse_multibulk-1"></a>

### parse_multibulk/1 ###

`parse_multibulk(Data) -> any()`

<a name="parse_multibulk-2"></a>

### parse_multibulk/2 ###

`parse_multibulk(X1, NewData0) -> any()`


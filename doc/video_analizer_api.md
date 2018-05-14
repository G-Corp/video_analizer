

# Module video_analizer_api #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-format">format()</a> ###


<pre><code>
format() = svg | csv | json
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {ffprobe, string()} | {stream, <a href="#type-stream">stream()</a>} | {width, integer()} | {height, integer()} | allowed_extensions | verbose | {output, string()} | {format, <a href="#type-format">format()</a>}
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>




### <a name="type-stream">stream()</a> ###


<pre><code>
stream() = video | audio
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#analyze-2">analyze/2</a></td><td>
Analyze the given <tt>Video</tt></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="analyze-2"></a>

### analyze/2 ###

<pre><code>
analyze(Video::string(), Options::<a href="#type-options">options()</a>) -&gt; ok
</code></pre>
<br />

Analyze the given `Video`


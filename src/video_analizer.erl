% @hidden
-module(video_analizer).

-export([main/1]).

-define(OPTS_SPEC,
        [
         {help,           $h, "help",           undefined,                   "Display this help"},
         {ffprobe,        $p, "ffprobe",        {string, "ffprobe"},         "FFProbe path"},
         {stream,         $s, "stream",         {string, "video"},           "Stream type (audio or video)"},
         {width,          $W, "width",          {integer, 800},              "Output image width"},
         {height,         $H, "height",         {integer, 600},              "Output image height"},
         {all_extensions, $A, "all_extensions", undefined,                   "Force ffprobe to accept all extensions"},
         {verbose,        $V, "verbose",        undefined,                   "Verbose mode"},
         {output,         $o, "output",         {string, "output.<format>"}, "Output file name (\"-\" for STDOUT)"},
         {format,         $f, "format",         {string, "svg"},             "Output format (svg, csv, json)"}
        ]).

main(Args) ->
  case display_help(getopt:parse(?OPTS_SPEC, Args)) of
    {false, Opts, [File]} ->
      video_analizer_api:analyze(File, update_output(Opts));
    true ->
      ok
  end.

display_help({ok, {Opts, Files}}) ->
  case lists:member(help, Opts) orelse length(Files) =/= 1 of
    true ->
      getopt:usage(?OPTS_SPEC, bucs:to_string(?MODULE)),
      true;
    false ->
      {false, Opts, Files}
  end;
display_help(_) ->
  getopt:usage(?OPTS_SPEC, bucs:to_string(?MODULE)),
  true.

update_output(Opts) ->
  File = proplists:get_value(output, Opts),
  Format = proplists:get_value(format, Opts),
  lists:keyreplace(output, 1, Opts, {output, re:replace(File, "<format>$", Format, [{return, list}])}).

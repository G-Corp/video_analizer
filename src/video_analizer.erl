-module(video_analizer).

-export([main/1]).

-define(OPTS_SPEC,
        [
         {help,           $h, "help",           undefined,              "Display this help"},
         {ffprobe,        $p, "ffprobe",        {string, "ffprobe"},    "FFProbe path"},
         {stream,         $s, "stream",         {string, "video"},      "Stream type (audio or video)"},
         {width,          $W, "width",          {integer, 800},         "Output image width"},
         {height,         $H, "height",         {integer, 600},         "Output image height"},
         {all_extensions, $A, "all_extensions", undefined,              "Force ffprobe to accept all extensions"},
         {verbose,        $V, "verbose",        undefined,              "Verbose mode"},
         {output,         $o, "output",         {string, "output.svg"}, "Output SVG file name"}
        ]).

main(Args) ->
  case display_help(getopt:parse(?OPTS_SPEC, Args)) of
    {false, Opts, [File]} ->
      video_analizer_api:analyze(File, Opts);
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

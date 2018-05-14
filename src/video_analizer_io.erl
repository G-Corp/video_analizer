-module(video_analizer_io).

-export([
         open/2
         , close/1
        ]).

open("-", _) -> {ok, standard_io};
open(Filename, Mode) -> file:open(Filename, Mode).

close(standard_io) -> ok;
close(IO) -> file:close(IO).

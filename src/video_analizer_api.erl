-module(video_analizer_api).

-export([
         analyze/2
        ]).

-define(PROBE_AE, "~s -v quiet -of json -show_entries ~ts -select_streams ~ts -allowed_extensions ALL \"~ts\"").
-define(PROBE_NA, "~s -v quiet -of json -show_entries ~ts -select_streams ~ts \"~ts\"").

-define(VERBOSE(Verbose, Format, Data), if
                                          Verbose -> io:format(Format, Data);
                                          true -> ok
                                        end).

-type stream() :: video | audio.
-type format() :: svg | csv | json.
-type options() :: [option()].
-type option() :: {ffprobe, string()}
                  | {stream, stream()}
                  | {width, integer()}
                  | {height, integer()}
                  | allowed_extensions
                  | verbose
                  | {output, string()}
                  | {format, format()}.

% @doc
% Analyze the given <tt>Video</tt>
% @end
-spec analyze(Video::string(), Options::options()) -> ok.
analyze(Video, Options) ->
  Verbose = proplists:get_value(verbose, Options),
  ?VERBOSE(Verbose, "Get all frames...~n", []),
  Frames = get_all_frames(Video, Options),
  ?VERBOSE(Verbose, "Get frame rate...~n", []),
  FrameRate = case bucs:to_string(proplists:get_value(stream, Options)) of
                "video" -> get_video_frame_rate(Video, Options);
                "audio" ->
                  [#{<<"pkt_duration_time">> := PktDurationTime}|_] = Frames,
                  1/bucs:to_float(PktDurationTime)
              end,
  ?VERBOSE(Verbose, "Build datas...~n", []),
  case build_datas(Frames, FrameRate, 1, 0.0, 0.0, 0.0, #{}) of
    {0, _, _, _} ->
      error("No frame data, failed to execute ffprobe");
    {FrameCount, MaxTime, MaxBitrate, BitrateData} ->
      ?VERBOSE(Verbose, "Build datas...~n", []),
      case bucs:to_string(proplists:get_value(format, Options)) of
        "svg" ->
          build_svg_output(
            BitrateData,
            FrameCount,
            proplists:get_value(output, Options),
            erlang:ceil(MaxTime), % MaxX
            erlang:ceil(MaxBitrate), % MaxY
            proplists:get_value(width, Options),
            proplists:get_value(height, Options),
            Verbose);
        "csv" ->
          build_csv_output(
            BitrateData,
            Verbose,
            proplists:get_value(output, Options));
        "json" ->
          build_json_output(
            BitrateData,
            Verbose,
            proplists:get_value(output, Options));
        _ ->
          error("Output format not supported")
      end
  end.

get_all_frames(Video, Options) ->
  FFProbe = proplists:get_value(ffprobe, Options),
  Stream = case bucs:to_string(proplists:get_value(stream, Options)) of
             "audio" -> "a";
             _ -> "V"
           end,
  Cmd = case proplists:get_value(all_extensions, Options) of
          true ->
            lists:flatten(io_lib:format(?PROBE_AE, [FFProbe, "frame", Stream, Video]));
          _ ->
            lists:flatten(io_lib:format(?PROBE_NA, [FFProbe, "frame", Stream, Video]))
        end,
  Data = os:cmd(Cmd),
  #{<<"frames">> := Frames} = jsx:decode(bucs:to_binary(Data), [return_maps]),
  Frames.

get_video_frame_rate(Video, Options) ->
  FFProbe = proplists:get_value(ffprobe, Options),
  Cmd = case proplists:get_value(all_extensions, Options) of
          true ->
            lists:flatten(io_lib:format(?PROBE_AE, [FFProbe, "stream", "V", Video]));
          _ ->
            lists:flatten(io_lib:format(?PROBE_NA, [FFProbe, "stream", "V", Video]))
        end,
  Data = os:cmd(Cmd),
  #{<<"streams">> := [#{<<"avg_frame_rate">> := FR}|_]} = jsx:decode(bucs:to_binary(Data), [return_maps]),
  [Divident, Divisor] = binary:split(FR, <<"/">>),
  bucs:to_integer(Divident) / bucs:to_integer(Divisor).

build_datas([], _FrameRate, FrameCount, _FrameTime, MaxTime, MaxBitrate, BitrateData) ->
  {FrameCount, MaxTime, MaxBitrate, BitrateData};
build_datas([#{<<"pkt_size">> := PktSize} = Frame|Frames],
            FrameRate,
            FrameCount,
            FrameTime,
            MaxTime,
            MaxBitrate,
            BitrateData) ->
  PictType = maps:get(<<"pict_type">>, Frame, <<"A">>),
  NewFrameTime = case Frame of
                   #{<<"best_effort_timestamp_time">> := BestEffortTimestantTime} -> bucs:to_float(BestEffortTimestantTime);
                   #{<<"pkt_pts_time">> := PktPtsTime} -> bucs:to_float(PktPtsTime);
                   #{<<"pkt_duration_time">> := PktDurationTime} when FrameCount > 1 -> FrameTime + bucs:to_float(PktDurationTime);
                   _ -> FrameTime
                 end,
  FrameBitrate = (bucs:to_float(PktSize) * 8 / 1000) * FrameRate,
  PictTypeData = maps:get(PictType, BitrateData, []),
  build_datas(
    Frames,
    FrameRate,
    FrameCount + 1,
    NewFrameTime,
    max(MaxTime, NewFrameTime),
    max(MaxBitrate, FrameBitrate),
    BitrateData#{PictType => [{NewFrameTime, FrameBitrate}|PictTypeData]}).

build_svg_output(BitrateData, FrameCount, Filename, MaxX, MaxY, Width, Height, Verbose) ->
  {GlobalMeanBitrate,
   _GlobalPeakBitrate,
   GraphData,
   Legends} = build_svg_output(
                [{<<"A">>, "#ff0000"},
                 {<<"B">>, "#0000ff"},
                 {<<"P">>, "#31b404"},
                 {<<"I">>, "#ff0000"}],
                Verbose,
                BitrateData,
                FrameCount,
                MaxX,
                MaxY,
                Width,
                Height,
                0.0,
                0.0,
                [],
                []
               ),
  ?VERBOSE(Verbose, "Build finalze image...~n", []),
  video_analizer_svg:write(Filename, Width, Height, Legends, 0, MaxX, 0, MaxY, bucs:to_float(GlobalMeanBitrate, 2), GraphData).

build_svg_output([], Verbose, _BitrateData, _FrameCount, _MaxX, _MaxY, _Width, _Height, GlobalPeakBitrate, GlobalMeanBitrate, GraphData, Legends) ->
  ?VERBOSE(Verbose, "global peak bitrate = ~p~n", [GlobalPeakBitrate]),
  ?VERBOSE(Verbose, "global mean bitrate = ~p~n", [GlobalMeanBitrate]),
  {GlobalMeanBitrate, GlobalPeakBitrate, GraphData, Legends};
build_svg_output([{FrameType, Color}|FrameTypes],
                 Verbose,
                 BitrateData,
                 FrameCount,
                 MaxX,
                 MaxY,
                 Width,
                 Height,
                 GlobalPeakBitrate,
                 GlobalMeanBitrate,
                 GraphData,
                 Legends) ->
  {NewGlobalPeakBitrate,
   NewGlobalMeanBitrate,
   NewGraphData,
   NewLegends} = case maps:get(FrameType, BitrateData, undefined) of
                   undefined ->
                     {GlobalPeakBitrate, GlobalMeanBitrate, GraphData, Legends};
                   FrameList ->
                     ?VERBOSE(Verbose, "~ts frames...~n", [FrameType]),
                     {GPB, GMB, GD} = build_frame_image(
                                        FrameList,
                                        Verbose,
                                        GlobalPeakBitrate,
                                        GlobalMeanBitrate,
                                        0.0,
                                        length(FrameList),
                                        FrameCount,
                                        Color,
                                        MaxX,
                                        MaxY,
                                        Width,
                                        Height,
                                        GraphData),
                     {GPB, GMB, GD,
                      [{lists:flatten(
                          io_lib:format(
                            "~ts frames",
                            [FrameType])),
                        Color}
                       |Legends]}
                 end,
  build_svg_output(
    FrameTypes,
    Verbose,
    BitrateData,
    FrameCount,
    MaxX,
    MaxY,
    Width,
    Height,
    NewGlobalPeakBitrate,
    NewGlobalMeanBitrate,
    NewGraphData,
    NewLegends).

build_frame_image([],
                  _Verbose,
                  GlobalPeakBitrate,
                  GlobalMeanBitrate,
                  FrameBitrateSum,
                  NbFrames,
                  FrameCount,
                  _Color,
                  _MaxX,
                  _MaxY,
                  _Width,
                  _Height,
                  GraphData) ->
  {GlobalPeakBitrate,
   GlobalMeanBitrate + (FrameBitrateSum / NbFrames) * (NbFrames / FrameCount),
   GraphData};
build_frame_image([{FrameTime, FrameBitrate}|FrameList],
                  Verbose,
                  GlobalPeakBitrate,
                  GlobalMeanBitrate,
                  FrameBitrateSum,
                  NbFrames,
                  FrameCount,
                  Color,
                  MaxX,
                  MaxY,
                  Width,
                  Height,
                  GraphData) ->
  build_frame_image(
    FrameList,
    Verbose,
    if
      GlobalPeakBitrate < FrameBitrate -> FrameBitrate;
      true -> GlobalPeakBitrate
    end,
    GlobalMeanBitrate,
    FrameBitrateSum + FrameBitrate,
    NbFrames,
    FrameCount,
    Color,
    MaxX,
    MaxY,
    Width,
    Height,
    [video_analizer_svg:bar(FrameTime, FrameBitrate, Width, Height, 0, MaxX, 0, MaxY, Color, [], false)|GraphData]
   ).

build_output_data(BitrateData) ->
  DefaultTimeData = lists:foldl(fun(V, Acc) ->
                  Acc#{V => 0.0}
              end, #{}, maps:keys(BitrateData)),
  maps:fold(fun(FrameType, Values, Acc) ->
                lists:foldl(fun({Time, Bitrate}, Acc0) ->
                                STime = bucs:to_float(Time, 3),
                                STimeData = maps:get(STime, Acc0, DefaultTimeData),
                                Acc0#{STime => STimeData#{FrameType => bucs:to_float(Bitrate, 3)}}
                            end, Acc, Values)
            end, #{}, BitrateData).

build_json_output(BitrateData, _Verbose, Filename) ->
  Datas = maps:fold(fun(K, V, Acc) ->
                [maps:put(<<"Time">>, K, V)|Acc]
            end, [], build_output_data(BitrateData)),
  Output = jsx:encode(Datas),
  {ok, IO} = video_analizer_io:open(Filename, [write]),
  file:write(IO, jsx:prettify(Output)),
  video_analizer_io:close(IO).

-define(CSV_LINE(Data), io_lib:format("~ts~n", [Data])).
build_csv_output(BitrateData, _Verbose, Filename) ->
  FinalData = build_output_data(BitrateData),
  CSVData = maps:fold(fun(Time, Data, Acc) ->
                [[bucs:to_string(Time) | [bucs:to_string(maps:get(FrameType, Data)) || FrameType <- maps:keys(BitrateData)]]|Acc]
            end, [], FinalData),
  SortedCSVData = lists:sort(fun([T0|_], [T1|_]) -> T0 < T1 end, CSVData),

  {ok, IO} = video_analizer_io:open(Filename, [write]),
  file:write(IO, ?CSV_LINE(string:join(["\"Time\""|[bucs:to_string(<<"\"", F/binary, " Frames\"">>) || F <- maps:keys(BitrateData)]], ";"))),
  [file:write(IO, ?CSV_LINE(string:join(L, ";"))) || L <- SortedCSVData],
  video_analizer_io:close(IO).

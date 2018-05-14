% @hidden
-module(video_analizer_svg).

-export([
         bar/11,
         write/10
        ]).

write(Filename, Width, Height, Legends, MinX, MaxX, MinY, MaxY, Mean, Data) ->
  SVG = svg:add(
          [
           % Background
           svg:rect({"100%", "100%"}, [{'fill', '#fff'}])
           | Data
          ] ++ [
                % Title
                svg:add(
                  svg:text({Width/2, 30}, "Stream Bitrate vs Time", [{'text-anchor', 'middle'}]),
                  svg:g([
                         {'font-family', 'Arial'},
                         {'font-size', 24},
                         {fill, '#000'},
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),

                % X Axis
                svg:add(
                  svg:path([{120, Height - 120}, {Width - 60, Height - 120}]),
                  svg:g([
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),
                svg:add(
                  svg:text({Width/2, Height - 25}, "Time (sec)", [{'text-anchor', 'middel'}]),
                  svg:g([
                         {'font-family', 'Arial'},
                         {'font-size', 24},
                         {fill, '#000'},
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),
                svg:add(
                  scale_x(Width, Height, MinX, MaxX, MinY, MaxY),
                  svg:g([
                         {'font-family', 'Arial'},
                         {'font-size', 12},
                         {fill, '#000'},
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),

                % Y Axis
                svg:add(
                  svg:path([{120, Height - 120}, {120, 120}]),
                  svg:g([
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),
                svg:add(
                  svg:text({30, Height/2}, "Frame Bitrate (kbps)", [{'text-anchor', 'middle'}, {transform, io_lib:format("rotate(270 30,~p)", [Height/2])}]),
                  svg:g([
                         {'font-family', 'Arial'},
                         {'font-size', 24},
                         {fill, '#000'},
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),
                svg:add(
                  scale_y(Width, Height, MinX, MaxX, MinY, MaxY),
                  svg:g([
                         {'font-family', 'Arial'},
                         {'font-size', 12},
                         {fill, '#000'},
                         {stroke, '#000'},
                         {'stroke-width', 1}
                        ])
                 ),

                % Legend
                svg:add(
                  svg:rect({{Width - 200, 50}, {150, 20 + 30 * (length(Legends))}}),
                  svg:g([
                         {fill, '#fff'},
                         {stroke, '#000'},
                         {'stroke-width', 3}
                        ])
                 ),
                svg:add(
                  legends_text(Legends, Width),
                  svg:g([
                         {'font-size', 24},
                         {'font-family', 'Arial'}
                        ])
                 ),

                % Mean
                svg:add(
                  [
                   svg:path([{120, pos_y(Mean, Height, MinY, MaxY)}, {Width - 60, pos_y(Mean, Height, MinY, MaxY)}], [{'stroke-width', 2}]),
                   svg:text({130, pos_y(Mean, Height, MinY, MaxY) - 10}, io_lib:format("Mean (~p)", [Mean]))
                  ],
                  svg:g([
                         {'font-size', 18},
                         {'font-family', 'Arial'},
                         {stroke, '#000'}
                        ])
                 )
               ],
          svg:init(Width, Height)
         ),
  write_to_file(Filename, SVG).

write_to_file("-", SVG) -> io:format("~ts~n", [iolist_to_binary(svg:export(SVG))]);
write_to_file(Filename, SVG) -> svg:write(Filename, SVG).

legends_text(Legends, Width) ->
  legends_text(Legends, Width, 85).

legends_text([], _Width, _Pos) ->
  [];
legends_text([{Text, Color}|Legends], Width, Pos) ->
  [
   svg:text({Width - 180, Pos}, Text, [{stroke, Color}]) |
   legends_text(Legends, Width, Pos + 30)
  ].

scale_x(Width, Height, MinX, MaxX, MinY, MaxY) ->
  Step = (MaxX - MinX) / 10,
  [
   svg:text({120, Height - 100}, bucs:to_string(MinX), [{'text-anchor', middle}]),

   svg:text({Width - 60, Height - 100}, bucs:to_string(bucs:to_float(MaxX, 2)), [{'text-anchor', middle}]),
   svg:path([{Width - 60, 120}, {Width - 60, Height - 120}], [{'stroke-dasharray', '5,5'}])
   | scale_x(Width, Height, MinX, MaxX, MinY, MaxY, Step, MinX + Step)
  ].

scale_x(Width, Height, MinX, MaxX, MinY, MaxY, Step, PosX) when PosX < MaxX ->
  [
   bar(PosX, MaxY, Width, Height, MinX, MaxX, MinY, MaxY, "#000", [{'stroke-dasharray', '5,5'}], true)
   | scale_x(Width, Height, MinX, MaxX, MinY, MaxY, Step, PosX + Step)
  ];
scale_x(_Width, _Height, _MinX, _MaxX, _MinY, _MaxY, _Step, _PosX) ->
  [].

scale_y(Width, Height, MinX, MaxX, MinY, MaxY) ->
  Step = (MaxY - MinY) / 10,
  [
   svg:text({110, Height - 120 + 4}, bucs:to_string(MinY), [{'text-anchor', 'end'}]),
   svg:text({110, 120 + 4}, bucs:to_string(bucs:to_float(MaxY, 2)), [{'text-anchor', 'end'}]),
   svg:path([{120, 120}, {Width - 60, 120}], [{'stroke-dasharray', '5,5'}])
   | scale_y(Width, Height, MinX, MaxX, MinY, MaxY, Step, MinY + Step)
  ].

scale_y(Width, Height, MinX, MaxX, MinY, MaxY, Step, PosY) when PosY < MaxY ->
  [
   line(MaxX, PosY, Width, Height, MinX, MaxX, MinY, MaxY, "#000", [{'stroke-dasharray', '5,5'}], true)
   | scale_y(Width, Height, MinX, MaxX, MinY, MaxY, Step, PosY + Step)
  ];
scale_y(_Width, _Height, _MinX, _MaxX, _MinY, _MaxY, _Step, _PosX) ->
  [].

bar(X, Y, Width, Height, MinX, MaxX, MinY, MaxY, Color, Args, DisplayValue) ->
  Bar = svg:add(
    svg:path(
      [{pos_x(X, Width, MinX, MaxX), Height - 120},
       {pos_x(X, Width, MinX, MaxX), pos_y(Y, Height, MinY, MaxY)}]
     ),
    svg:g([{fill, Color}, {stroke, Color} | Args])
   ),
  case DisplayValue of
    false ->
      Bar;
    true ->
      svg:add(
        [
         Bar,
         svg:text(
           {pos_x(X, Width, MinX, MaxX), Height - 100},
           bucs:to_string(bucs:to_float(X, 2)),
           [{'text-anchor', middle}])
        ],
        svg:g([
                {'font-family', 'Arial'},
                {'font-size', 12},
                {fill, Color},
                {stroke, Color},
                {'stroke-width', 1}
              ])
       )
  end.

line(X, Y, Width, Height, MinX, MaxX, MinY, MaxY, Color, Args, DisplayValue) ->
  Bar = svg:add(
    svg:path(
      [{120, pos_y(Y, Height, MinY, MaxY)},
       {pos_x(X, Width, MinX, MaxX), pos_y(Y, Height, MinY, MaxY)}]
     ),
    svg:g([{fill, Color}, {stroke, Color} | Args])
   ),
  case DisplayValue of
    false ->
      Bar;
    true ->
      svg:add(
        [
         Bar,
         svg:text(
           {110, pos_y(Y, Height, MinY, MaxY) + 4},
           bucs:to_string(bucs:to_float(Y, 2)),
           [{'text-anchor', 'end'}])
        ],
        svg:g([
                {'font-family', 'Arial'},
                {'font-size', 12},
                {fill, Color},
                {stroke, Color},
                {'stroke-width', 1}
              ])
       )
  end.

pos_y(Value, Height, MinY, MaxY) ->
  Height - 120 - (Value - MinY) * ((Height - 240) / (MaxY - MinY)).

pos_x(Value, Width, MinX, MaxX) ->
  120 + (Value - MinX) * ((Width - 180) / (MaxX - MinX)).

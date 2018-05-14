

# Bitrate Graph #

Copyright (c) 2017-2018 G-Corp

__Version:__ 0.1.0

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).


### video_analizer ###

This project was inspired by [plotbitrate](https://github.com/zeroepoch/plotbitrate).


#### Build and usage ####

```

$ make
...
$ ./_build/default/bin/video_analizer --help
Usage: video_analizer [-h] [-p [<ffprobe>]] [-s [<stream>]] [-W [<width>]]
                      [-H [<height>]] [-A] [-V] [-o [<output>]]
                      [-f [<format>]]

  -h, --help            Display this help
  -p, --ffprobe         FFProbe path [default: ffprobe]
  -s, --stream          Stream type (audio or video) [default: video]
  -W, --width           Output image width [default: 800]
  -H, --height          Output image height [default: 600]
  -A, --all_extensions  Force ffprobe to accept all extensions
  -V, --verbose         Verbose mode
  -o, --output          Output file name ("-" for STDOUT) [default:
                        output.<format>]
  -f, --format          Output format (svg, csv, json) [default: svg]

```


#### Output example ####

![](output.svg.png)


#### Licence ####

video_analizer is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2017-2018 G-Corp<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="video_analizer_api.md" class="module">video_analizer_api</a></td></tr></table>


hs-euo
======

hs-euo is a Haskell wrapper for [EasyUO], a tool that manipulates the [Ultima Online] client.. It provides a foreign interface with a direct mapping to the calls in the `uo.dll` library along with higher level functions for interacting with the Ultima Online client.

Note: this is still a big work in progress. The API is going to change. I'm open to any feedback.

### Getting Started

You will need to download the `uo.dll` library from [OpenEUO]. You specifically want the "OpenEUO" package. When you unzip it you should fine `uo.dll` in there. That will need to be added to your library path when building the project. You can do that by passing `--extra-lib-dirs` to stack (or cabal):

    stack build --extra-lib-dirs=C:\Path\to\dll

### Copyright

Copyright (c) 2016 David Tulig. See LICENSE for details.

[EasyUO]: http://www.easyuo.com/
[OpenEUO]: http://www.easyuo.com/forum/viewtopic.php?t=52615
[Ultima Online]: http://uo.com/
Not a real project. Just to test rebuilds of TH files in ide-backend.

Issues:  
- Pollutes your working directory with ghc dump files.

How to use:

- clone this repo and cd into it.
- run `stack build`.
- create a new yesod scaffold wherever you wish and cd into it.
- run `stack build`
- create a symlink from to the devel binary you built earlier to your scaffold's root dir. e.g `ln -sf ~/src/haskell/devel/.stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/devel .`
- run `./devel` to use

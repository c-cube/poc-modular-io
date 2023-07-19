#!/bin/sh
dune build --display quiet examples/multicat.exe 2>/dev/null || true
exec ./_build/default/examples/multicat.exe $@

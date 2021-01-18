# idris-fix-whitespace
Removes trailing whitespace from .idr files and makes sure they
end with exactly one newline.

Use the following to build:

```
idris2 --build fix-whitespace.ipkg
```

The executable can be found at `build/exec/fix_whitespace`. For a list of
command line options, run

```
build/exec/fix_whitespace --help
```

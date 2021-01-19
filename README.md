# idris-fix-whitespace
Removes trailing whitespace from .idr files and makes sure they
end with exactly one newline.

## Synopsis
Removes trailing whitespace characters from the specified
text files making sure every text file ends with exactly one
newline character. Windows style line breaks are replaced
by Unix newline characters.

If the passed file list contains directories, `fix_whitespace`
will recursively adjust all files with the given extensions
(see option --ext) in those directories. If no files are
specified, the current directory will be traversed instead.

```
Usage: fix_whitespace [options] [FILES]

Options:


  -h         --help           prints this help text
  -v         --verbose        increase verbosity (default verbosity is 2)

  -q         --quiet          decrease verbosity (default verbosity is 2)

  -c         --check          check and list files with issues (default)

  -f         --fix            check and fix files with issues

  -e <exts>  --ext=<exts>     comma separated list of extensions of files
                              to be included when traversing directories
                              (default is "idr").
                              Note: Files whose name starts with a dot will be
                              ignored unless '--includeHidden' is set.

  -a         --all            include all non-hidden files when traversing directories

             --includeHidden  include hidden files (name starts with a dot)
                              when traversing directories
```

## Building
This has only been tested against Idris2 version 0.3.0.

Use the following to build:

```
idris2 --build fix-whitespace.ipkg
```

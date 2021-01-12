# poc-modular-io

proof of concept for https://github.com/ocaml/RFCs/pull/19

## Examples

```sh
$ echo "hello\nworld\n🤗" > foo

$ ./multicat.sh foo foo foo -rot13
uryyb\ajbeyq\a🤗
uryyb\ajbeyq\a🤗
uryyb\ajbeyq\a🤗

$ ./multicat.sh foo foo foo -rot13 > bar

$ ./multicat.sh bar -rot13 -rot13 -rot13  # triple decode!
hello\nworld\n🤗
hello\nworld\n🤗
hello\nworld\n🤗
```

The following example reads a file, translates it via "rot13", then uses
the http1.1 chunked encoding; then the rest of the pipeline decodes the
chunked encoding and rot13.

Note that the pipeline runs in constant memory, it does
not need to read the whole file to do its translations.

```sh
$ cat src/IO_helpers.ml
… # some content

$ ./multicat.sh src/IO_helpers.ml -rot13 -chunk
… # content, but less readable

$ ./multicat.sh src/IO_helpers.ml -rot13 -chunk | ./multicat.sh -unchunk -rot13
… # initial content of the file

```

## Bugs

`-zip` and `-unzip` don't seem to work yet, I am not familiar enough with camlzip.

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

Note: `-zip` and `-unzip` don't seem to work yet.

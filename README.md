# cl-indentify

A library and command line utility to automatically indent Common Lisp source
files.

# Installation

For command line usage cl-indentify currently requires [roswell][]. Once
roswell is installed enter the following in a shell.

```sh
$ ros install yitzchak/cl-indentify
```

# Functionality

Inspired by [scmindent/lispindent][] cl-indentify attempts to deal with more
indentation involved issues with a template system that can specify the
indention characteristics of subforms in addition to the top level form.

# Command Line Usage

Using cl-indentify as a stdin/stdout filter is simple.

```sh
$ echo bar.lisp | cl-indentify
```

To read directly from a file just specify the filename on the command line.

```sh
$ cl-indentify bar.lisp
```

The output can be sent to a specific file versus `stdout` via the `--outfile`
option.

```sh
$ cl-indentify -o fu.lisp bar.lisp
```

Multiple files can be processed at one time by specifying them on the command
line. The output will be sent to the file in the `--outfile` option or `stdout`
unless the `--replace` option is used to replace the original files.

```sh
$ cl-indentify -r fu.lisp bar.lisp
```

## Configuration

[scmindent/lispindent]: https://github.com/ds26gte/scmindent
[roswell]: https://github.com/roswell/roswell

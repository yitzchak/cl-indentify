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
complex indention by using with a template system that can specify the
indention characteristics of subforms in addition to the top level form.

cl-indentify calculates indention based on the number of subforms in a "primary"
group and the number of subforms in a "secondary" group. For example, `do*`
is defined to have 2 subforms in the primary group and the remaining in th
secondary group. The primary group is indented by four columns and the secondary
group is indented by two columns by default.

```common-lisp
(do*
    ((fu 0 (+1 fu))) 
    ((= fu 10) bar)
  wibble
  quux)
```

If the first subform is on the same line as `do*` the following subforms
that begin on a newline are aligned with the first subform.

```common-lisp
(do* ((fu 0 (+1 fu))) 
     ((= fu 10) bar)
  wibble
  quux)
```

Likewise, if the first secondary subform is on the same line as the last primary
subform the following subforms that begin on a newline are aligned with the 
first secondary subform. This is demonstrated by `if` which has a primary
count of 1. When the "then" clause is on its own line the following is the
result.

```common-lisp
(if fu
  bar
  quux)
```

When the "then" class is on the same line as the test expression then the "else"
clause will be aligned to the "then" clause.


```common-lisp
(if fu bar
       quux)
```

If the first subform in a list is determined to a literal, a list, or if the
form is quoted then indention will be by one column so that all items aligned
after the leading left paren.

```common-lisp
(1
 2
 3)

'(fu
  bar
  3)
```

Becuase the indention characteristics of subforms can also be specified this
behavior can be overriden so that forms such as `cond` or `case` will have a
primary count of one even if the leading subform is a literal.

```common-lisp
(cond
  ((eql fu 'bar)
    wibble)
  (t
    quux))
    
(case gronk
  (1 baz
     zap)
  (743
    zip
    bang))
```    

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

Unless the `--no-defaults` option is specified then cl-indentify will load
a set of default templates. After these templates are loaded then cl-indentify
will look in the user's config home for a template file according the XDG
specification. For instance, on Linux this would be
`~/.config/cl-indentify/templates.lisp`, which can be supressed by using the
`--no-user` option. Additional template files can be loaded using the
`--templates` options. For example, the following suppresses defaults and user
templates and loads the template files `wibble` and `quux`.

```sh
$ cl-indentify --no-defaults --no-user --templates wibble -t quux fu.lisp
```

Template files should be in Sexpr format with each item a list that specifies
the indention characteristics of a function or macro. For instance, the 
following specifies that `defun` has a primary count of two. It also
specifies that `defmethod` has a primary count of two but that quantifiers
`:before`, `:after`, and `:around` should be ignored in accumulating the primary
count. `case` is defined to have a primary count of one and secondary forms
are defined to have a primary count of zero.

```common-lisp
(defun :count 2)
(defmethod :count 2 :ignore (:before :after :around))
(case :count 1 :secondary (:count 0))
```

Templates can be nested so the following would be legal.

```common-lisp
(fu :count 1 :primary (:count 2) :secondary (:count 0 :secondary (:count 1)))
```

[scmindent/lispindent]: https://github.com/ds26gte/scmindent
[roswell]: https://github.com/roswell/roswell

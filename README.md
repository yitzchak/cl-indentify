# cl-indentify

A library and command line utility to automatically indent Common Lisp source files.

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

Templates are generally selected by the head form of a list. For example, in
the following cl-indentify will attempt to select a template with a key name
of `"LET"`.

```common-lisp
(let
    ((fu 1)
     (bar 2))
  (+ fu bar))
```

Templates can configure a number of aspects of the indention characteristic, but
the main behavior is controlled by the template `:style` key. 

## `:call` Style

The default style
is `:call` which calculates indention based a fixed length "primary" group and
a variable "secondary" group. For example, `with-slots` is defined to have two subforms 
in the primary group and the remaining in the secondary group. The primary group 
is indented by four columns and the secondary group is indented by two columns 
by default.

```common-lisp
(with-slots
    (fu bar) wibble
  (quux bar)
  (+ fu bar))
```

If the first subform is on the same line as `with-slots` the following subforms
that begin on a newline are aligned with the first subform.

```common-lisp
(with-slots (fu bar)
            wibble
  (quux bar)
  (+ fu bar))
```

Likewise, if the first secondary subform is on the same line as the last primary
subform the following subforms that begin on a newline are aligned with the 
first secondary subform. This can be seen in the case of `if` which has a
primary count of one. Normally would be indented as

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

If a template cannot be found for the head subform then the `:call` style is
assumed with a primary count of zero.

The template for subforms can be explicitly specified using the `:sub` key.
The value is a list of templates or a `nil` if the default template should be
used. For `:call` and `:tag` style the first item in the list should always be
a `nil` as this is the template for head subform. This is only used for `:list`
styles. If the length of the `:sub` value is less then the length of the form
then the last item in `:sub` will be used. 

For example, the template for `cond` is the following, which specifies that all
secondary group subforms will have a style of `:call` with a primary count of
zero.

```common-lisp
(:style :call
 :count 0
 :sub (nil
       (:style :call
        :count 0)))
```

This results in

```common-lisp
(cond
  ((not ch)
    :eof)
  ((char= ch #\!)
    :bang)
  (t
    ch))
```

## `:tag` Style

`:tag` style behaves exactly as `:call` in respect to primary and secondary 
groups, but will align tag names that appear in special forms such as `tagbody` 
with the head subform. For example `do*`

```common-lisp
(do* ((pos 0 (1+ pos))
      (wibble 7))
     ((= pos 20))
  (when (= pos 20)
    (go fu))
  (format t "~A~%" pos)
 fu
  (format t "~A~%" wibble))
```

## `:quote` Style

`:quote` style will indent the form and any subforms as if it a quoted literal.
List subforms will aligned the head subform.

```common-lisp
(1
 2
 3)

'(fu bar
  3
  (wibble
   quux))
```

## `:list` Style

`:list` style is like `:quote` style but subforms will be indented according
to their own templates or may be specified by the `:sub` key as in `:call`. For
example, `let` has its primary subform style as `:list` with the following 
template

```common-lisp
(:style :call
 :count 1
 :sub (nil
       (:style :list
        :sub ((:style :call :count 0)))
       nil))
```

This results in

```common-lisp
(let (quux
      (fu 1)
      (bar
        (wibble 7)))
  (+ fu bar))
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
(defun :style :call
       :count 2
       :sub (nil nil
             (:style :list)
             nil))

(defmethod :style :call
           :count 2 
           :ignore (:before :after :around) 
           :sub ((:style :list)
                 (:style :list)
                 nil))

(case :style :call
      :count 1 
      :sub (nil nil 
            (:style :call :count 0)))
```

[scmindent/lispindent]: https://github.com/ds26gte/scmindent
[roswell]: https://github.com/roswell/roswell

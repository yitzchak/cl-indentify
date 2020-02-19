(defun fu (wibble &optional quux)
  (baz wibble
       (quux
         bar)
       '(1 2
         4)))

(case ; Oddly placed comment
    fu
  (1 nil)
  (2 t)
  (3 5))

(block
  a
  b)

(if a
  b
  c)

(if
    a
  b
  c)

(if a b
      c)

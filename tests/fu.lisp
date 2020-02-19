(defun fu (wibble &optional quux)
  (baz wibble
       (quux
         bar)
       '(1 2
         4))
  (apply #'quux
         1 2 '(a b)))

(case ; Oddly placed comment
    fu
  (#\) nil)
  (#\Space
   t)
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

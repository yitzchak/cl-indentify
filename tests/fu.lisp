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

(defun #| block comment |#
    bar (x)
  (cond
    ((numberp x)
      (1+ x))
    (t
      x)))

(defclass foo ()
  ((wibble
     :accessor wibble
     :initarg :wibble)
   (gronk :accessor gronk
          :initarg :gronk)))

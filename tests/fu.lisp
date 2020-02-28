(defun fu (wibble &optional quux)
  (baz wibble
       (quux
         bar)
       '(1 2
         4))
  (apply #'quux
         1 2 '(a b)))

(defun wibble
       (bar zap)
  (expt bar zap))

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

(let (e
      (f 1)
      (g (fu 2
             3)))
  (print f))

(let* ((f 1)
       (g 1))
  (print f))

(do* ((f 1 (1+ f)))
     ((> f 4) 'bar)
  (print f))

'(wibble
  quux 1
  2 3)

(defmethod stop ((hb hb-channel))
  (bordeaux-threads:destroy-thread (hb-thread-id hb)))

(defmethod stop :after ((hb hb-channel))
  (wibble))

(defun scan-indent (stream &optional template)
  (declare (ignore template))
  (setf (echo stream) nil)
  (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((or (not ch)
           (and (char/= ch #\Space)
                (char/= ch #\Tab))))
    (read-char stream nil))
  (setf (echo stream) t)
  '(:indent))


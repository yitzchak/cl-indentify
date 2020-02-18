(in-package :cl-indentify)

(defmacro dostring ((var string-form &optional resultform) &rest body)
  (with-gensyms (string-val pos)
    `(let ((,string-val ,string-form))
	     (dotimes (,pos (length ,string-val) ,resultform)
	       (let ((,var (char ,string-val ,pos))) ,@body)))))

(defclass indenter ()
  ((indent-numbers
     :reader indent-numbers
     :initform (make-hash-table :test 'equalp))
   (tab-size
     :accessor tab-size
     :initform 8
     :initarg :tab-size)))

(defclass indenter-state ()
  ((paren-stack
     :accessor paren-stack
     :initform nil)
   (input-stream
     :accessor input-stream
     :initarg :input-stream)
   (output-stream
     :accessor output-stream
     :initarg :output-stream)
   (newline
     :accessor newline
     :initarg t)
   (current-indent
     :accessor current-indent
     :initform 0)
   (token-interstice
     :accessor token-interstice
     :initform nil)
   (in-comment
     :accessor in-comment
     :initform nil)
   (in-string
     :accessor in-string
     :initform nil)))

(defun imported-symbol-name (name)
  (when-let ((pos (position #\: name)))
    (subseq name (1+ pos))))

(defun indent-number (instance name)
  (with-slots (indent-numbers) instance
    (let ((normalized-name (uiop:standard-case-symbol-name name)))
      (or (gethash normalized-name indent-numbers)
          (gethash (imported-symbol-name name) indent-numbers)
          -1))))

(defun (setf indent-number) (value instance name)
  (setf (gethash (uiop:standard-case-symbol-name name) (indent-numbers instance)) value))

(defparameter +default-indent-numbers+
  '((assert 2)
    (block 0)
    (case 1)
    (defclass 1)
    (defconstant 1)
    (defgeneric 2)
    (define-compiler-macro 2)
    (define-condition 1)
    (define-method-combination 2)
    (define-modify-macro 2)
    (define-setf-expander 2)
    (define-symbol-macro 2)
    (defmacro 2)
    (defmethod 2)
    (defpackage 1)
    (defparameter 1)
    (defsetf 2)
    (defstruct 1)
    (deftype 1)
    (defun 2)
    (defvar 1)
    (destructuring-bind 2)
    (do 2)
    (do* 2)
    (do-all-symbols 1)
    (do-external-symbols 1)
    (dolist 1)
    (do-symbols 1)
    (dotimes 1)
    (ecase 1)
    (etypecase 1)
    (eval-when 1)
    (flet 1)
    (handler-bind 0)
    (handler-case 1)
    (if 1)
    (labels 1)
    (lambda 1)
    (let 1)
    (let* 1)
    (let-values 1)
    (loop 0)
    (macrolet 1)
    (multiple-value-bind 2)
    (prog1 1)
    (typecase 1)
    (unless 1)
    (unwind-protect 1)
    (when 1)
    (with-input-from-string 1)
    (with-open-file 1)
    (with-open-socket 1)
    (with-open-stream 1)
    (with-output-to-string 1)
    (with-slots 2)))

(defun load-default-indents (instance)
  (dolist (p +default-indent-numbers+)
    (setf (indent-number instance (car p)) (cadr p))))

(defun load-user-indents (instance)
  (with-open-file (indents-stream (uiop:xdg-config-home "cl-indentify" "indents.lisp") :if-does-not-exist nil)
    (when indents-stream
      (do ((q (read indents-stream nil) (read indents-stream nil)))
          ((not q))
        (let ((num (car (last q))))
          (dolist (name (butlast q))
            (setf (indent-number instance name) num)))))))

(defun past-next-atom (s i n)
  (do ((j i (1+ j)))
      ((>= j n) n)
    (case (char s j)
      (#\\
       (incf j))
      ((#\space #\tab #\( #\) #\[ #\] #\" #\' #\` #\, #\;)
       (return j)))))

(defun literal-token-p (s)
  (let ((colon-pos (position #\: s)))
    (if colon-pos
      (= colon-pos 0)
      (let ((s (read-from-string s)))
        (or (characterp s) (numberp s) (stringp s))))))

(defclass lparen ()
  ((spaces-before
     :accessor spaces-before
     :initform 0
     :initarg :spaces-before)
   (lisp-indent-num
     :accessor lisp-indent-num
     :initform 0
     :initarg :lisp-indent-num)
   (num-finished-subforms
     :accessor num-finished-subforms
     :initform -1
     :initarg :num-finished-subforms)))

(defun calc-subindent (instance s i n)
  (let* ((j (past-next-atom s i n))
         (lisp-indent-num 0)
         (delta-indent
           (if (= j i) 0
             (let ((w (subseq s i j)))
               (if (or (and (>= i 2) (member (char s (- i 2)) '(#\' #\`)))
                       (literal-token-p w)) 0
                 (progn (setq lisp-indent-num (indent-number instance w))
                        (case lisp-indent-num
                          ((-2) 0)
                          ((-1) (if (< j n) (+ (- j i) 1) 1))
                          (t 1))))))))
    (values delta-indent lisp-indent-num j)))

(defun num-leading-spaces (s)
  (let ((n (length s))
        (i 0) (j 0))
    (loop
      (when (>= i n) (return 0))
      (case (char s i)
        (#\space (incf i) (incf j))
        (#\tab (incf i) (incf j 8))
        (t (return j))))))

(defun remove-indent (instance line)
  (with-slots (tab-size) instance
    (do ((pos 0 (1+ pos))
         (indent-count 0))
        ((>= pos (length line)) (values indent-count ""))
      (case (char line pos)
        (#\space (incf indent-count))
        (#\tab (incf indent-count tab-size))
        (otherwise (return (values indent-count (subseq line pos))))))))

(defun string-trim-blanks (s)
  (string-trim '(#\space #\tab #\newline #\return) s))

(defun calculate-line-indent (instance state leading-spaces)
  (declare (ignore instance))
  (with-slots (paren-stack in-string current-indent) state
    (cond
      (in-string leading-spaces)
      ((null paren-stack)
        (if (zerop current-indent)
          (setf current-indent leading-spaces)
          current-indent))
      (t
        (with-slots (spaces-before lisp-indent-num num-finished-subforms) (car paren-stack)
          (cond
            ((< num-finished-subforms lisp-indent-num) ;(and (>= lisp-indent-num 0) (< num-finished-subforms lisp-indent-num))
              (incf num-finished-subforms)
              (+ spaces-before 2))
            (t
              spaces-before)))))))

(defun incr-finished-subforms (instance state)
  (declare (ignore instance))
  (with-slots (token-interstice paren-stack) state
    (unless token-interstice
      (when paren-stack
        (incf (num-finished-subforms (car paren-stack))))
      (setf token-interstice t))))

(defun scan-line (instance state line-indent curr-line)
  (with-slots (paren-stack token-interstice current-indent in-string) state
    (setf token-interstice nil)
    (do ((pos 0 (1+ pos)))
        ((>= pos (length curr-line)))
      (let ((ch (char curr-line pos)))
        (cond
          ((char= ch #\\)
            (setf token-interstice nil)
            (incf pos))
          (in-string
            (when (char= ch #\")
              (setf in-string nil)
              (incr-finished-subforms instance state)))
          ((char= ch #\;)
            (incr-finished-subforms instance state)
            (return))
          ((char= ch #\")
            (incr-finished-subforms instance state)
            (setf in-string t))
          ((member ch '(#\space #\tab) :test #'char=)
            (incr-finished-subforms instance state))
          ((or (char= ch #\() (char= ch #\[))
            (incr-finished-subforms instance state)
            (multiple-value-bind (delta-indent lisp-indent-num j)
                                 (calc-subindent instance curr-line (1+ pos) (length curr-line))
              (push (make-instance 'lparen :spaces-before (+ 1 pos line-indent delta-indent)
                                           :lisp-indent-num lisp-indent-num)
                    paren-stack)
              (setf token-interstice t)
              (let ((inext (1+ pos)))
                (when (> j inext)
                  (setq inext j)
                  (setf token-interstice nil))
                (setq pos (1- inext)))))
          ((member ch '(#\) #\]) :test #'char=)
            (setf token-interstice nil)
            (if paren-stack
              (pop paren-stack)
              (setf current-indent 0)))
          (t
            (setf token-interstice nil))))
      (incr-finished-subforms instance state))))

(defun indent-lines (instance)
  (do ((state (make-instance 'indenter-state))
       (line (read-line nil nil) (read-line nil nil)))
      ((null line))
    (multiple-value-bind (leading-spaces curr-line) (remove-indent instance line)
      (let ((line-indent (calculate-line-indent instance state leading-spaces)))
        (dotimes (k line-indent)
          (write-char #\space))
        (princ curr-line)
        (terpri)
        (scan-line instance state line-indent curr-line)))))

(defun scan-char (state &key (echo t))
  (when-let ((ch (read-char (input-stream state) t)))
    (if echo
      (write-char ch (output-stream state))
      ch)))

(defun scan-string (instance state)
  (scan-char state)
  (do ((ch (scan-char state) (scan-char state)))
      ((char= ch #\"))
    (when (char= ch #\\)
      (scan-char state)))
  (incr-finished-subforms instance state))

(defun scan-line-comment (instance state)
  (declare (ignore instance))
  (do ((ch (scan-char state) (scan-char state)))
      ((not ch))
    (when (char= ch #\Newline)
      (setf (newline state) t)
      (return t))))

(defun scan-indent (instance state)
  (with-slots (input-stream output-stream) state
    (do ((ch (peek-char nil input-stream nil) (peek-char nil input-stream nil))
         (indent 0))
        ((not ch))
      (case ch
        (#\Space
          (read-char input-stream nil)
          (incf indent))
        (#\Tab
          (read-char input-stream nil)
          (incf indent (tab-size instance)))
        (otherwise
          (dotimes (k (calculate-line-indent instance state indent))
            (write-char #\Space output-stream))
          (return t))))))

;(defun indentify (instance &optional input-stream output-stream)
;  (do ((state (make-instance 'indenter-state :input-stream (or input-stream *standard-input*)
;                                             :output-stream (or output-stream *standard-output*)))
;       (ch (peek-char nil input-stream nil)

;  (do ((ch (read-char input-stream t) (read-char input-stream t))
;       (in-indent t)
;       (in-string nil)
;       (in-comment nil)
;       (current-indent 0))
;    (if in-indent

    

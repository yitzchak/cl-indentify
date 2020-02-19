(in-package :cl-indentify)

(defclass indenter ()
  ((indent-numbers
     :reader indent-numbers
     :initform (make-hash-table :test 'equalp))
   (tab-size
     :accessor tab-size
     :initform 8
     :initarg :tab-size)))

(defclass indenter-state ()
  ((input-stream
     :accessor input-stream
     :initarg :input-stream)
   (output-stream
     :accessor output-stream
     :initarg :output-stream)
   (column
     :accessor column
     :initform 0)))

(defun literal-token-p (s)
  (let ((colon-pos (position #\: s)))
    (if colon-pos
      (= colon-pos 0)
      (let ((s (read-from-string s)))
        (or (characterp s) (numberp s) (stringp s))))))

(defun imported-symbol-name (name)
  (when-let ((pos (position #\: name)))
    (subseq name (1+ pos))))

(defun indent-number (instance name)
  (unless (literal-token-p name)
    (with-slots (indent-numbers) instance
      (let ((normalized-name (uiop:standard-case-symbol-name name)))
        (or (gethash normalized-name indent-numbers)
            (gethash (imported-symbol-name name) indent-numbers)
            0)))))

(defun (setf indent-number) (value instance name)
  (setf (gethash (uiop:standard-case-symbol-name name) (indent-numbers instance)) value))

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

(defun scan-char (instance state &key (echo t))
  (with-slots (input-stream output-stream column) state
    (when-let ((ch (read-char input-stream nil)))
      (case ch
        (#\Tab
          (incf column (tab-size instance)))
        (#\Newline
          (setf column 0))
        ((#\Backspace #\Page #\Return #\Rubout))
        (otherwise
          (incf column)))
      (if echo
        (write-char ch output-stream)
        ch))))

(defun unscan-char (instance state ch)
  (with-slots (column) state
    (unread-char ch (input-stream state))
    (case ch
      (#\Tab
        (decf column (tab-size instance)))
      ((#\Backspace #\Newline #\Page #\Return #\Rubout))
      (otherwise
        (decf column)))))

(defun scan-string (instance state)
  (scan-char instance state)
  (do ((ch (scan-char instance state) (scan-char instance state)))
      ((not ch))
    (case ch
      (#\\
        (scan-char instance state))
      (#\"
        (return '(:form))))))

(defun scan-indent (instance state)
  (with-slots (column output-stream) state
    (do ((ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
        ((not ch))
      (case ch
        ((#\Space #\Tab))
        (otherwise
          (unscan-char instance state ch)
          (return)))))
  '(:indent))

(defun scan-line-comment (instance state)
  (do ((ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
      ((not ch))
    (when (char= ch #\Newline)
      (unscan-char instance state ch)
      (return))
    (write-char ch (output-stream state)))
  '(:space))

(defun scan-token (instance state)
  (list
    :form
    (with-output-to-string (token-stream)
      (do ((ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
          ((not ch))
        (case ch
          ((#\Space #\Newline #\( #\) #\' #\` #\, #\@ #\;)
            (unscan-char instance state ch)
            (return))
          (otherwise
            (write-char ch token-stream)
            (write-char ch (output-stream state))))))))

(defun scan-sharpsign-backslash (instance state)
  (do ((pos 0 (1+ pos))
       (ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
      ((not ch))
    (case ch
      ((#\Space #\Newline)
        (unscan-char instance state ch)
        (return))
      ((#\( #\) #\' #\` #\, #\@ #\;)
        (if (zerop pos)
          (write-char ch (output-stream state))
          (progn
            (unscan-char instance state ch)
            (return))))
      (otherwise
        (write-char ch (output-stream state)))))
  '(:form))

(defun scan-sharpsign (instance state)
  (let ((ch (scan-char instance state)))
    (case ch
      (#\\
        (scan-sharpsign-backslash instance state))
      (otherwise
        '(:form)))))

(defun scan-chunk (instance state)
  (with-slots (output-stream) state
    (let ((ch (scan-char instance state :echo nil)))
      (case ch
        (#\Newline
          (write-char ch (output-stream state))
          (scan-indent instance state))
        (#\"
          (write-char ch (output-stream state))
          (scan-string instance state))
        (#\;
          (write-char ch (output-stream state))
          (scan-line-comment instance state))
        (#\#
          (write-char ch (output-stream state))
          (scan-sharpsign instance state))
        ((#\Space #\Tab)
          (write-char ch (output-stream state))
          '(:space))
        ((#\' #\` #\@ #\,)
          (write-char ch (output-stream state))
          (scan-form instance state))
        ((#\( #\[)
          (write-char ch (output-stream state))
          (scan-forms instance state))
        ((#\) #\])
          (write-char ch (output-stream state))
          '(:exit))
        (otherwise
          (unscan-char instance state ch)
          (scan-token instance state))))))

(defun scan-form (instance state)
  (with-slots (output-stream) state
    (do ((chunk (scan-chunk instance state) (scan-chunk instance state)))
        ((not chunk))
      (unless (eql (car chunk) :space)
        (return chunk)))))

(defun scan-forms (instance state)
  (with-slots (column output-stream) state
    (do* ((indent column)
          (primary-indent (+ 3 indent))
          (secondary-indent (1+ indent))
          (primary-form-count nil)
          (completed-form-count 0)
          (previous-column column column)
          (form (scan-chunk instance state) (scan-chunk instance state)))
         ((not form))
      (case (car form)
        (:indent
          (setf column
            (cond
              ((not primary-form-count) indent)
              ((> completed-form-count primary-form-count)
                secondary-indent)
              (t primary-indent)))
          (dotimes (k column)
            (write-char #\Space output-stream)))
        (:form
          (incf completed-form-count)
          (cond
            ((and (= 1 completed-form-count) (cadr form))
              (setf primary-form-count (indent-number instance (cadr form))))
            ((and primary-form-count (= 1 completed-form-count))
              (setf primary-indent previous-column))
            ((and primary-form-count (= (+ 2 primary-form-count) completed-form-count))
              (setf secondary-indent previous-column))))
        (:exit
          (return '(:form)))))))

(defun indentify (instance &optional input-stream output-stream)
  (scan-forms
    instance
    (make-instance 'indenter-state
                    :input-stream (or input-stream *standard-input*)
                    :output-stream (or output-stream *standard-output*))))


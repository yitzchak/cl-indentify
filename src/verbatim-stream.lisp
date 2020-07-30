(in-package :indentify)

(defclass verbatim-stream (trivial-gray-streams:fundamental-character-output-stream
                           trivial-gray-streams:fundamental-character-input-stream)
  ((input-stream
     :accessor input-stream
     :initarg :input-stream)
   (output-stream
     :accessor output-stream
     :initarg :output-stream)
   (input-column
     :accessor input-column
     :initarg :input-column
     :initform 0)
   (output-column
     :accessor output-column
     :initarg :output-column
     :initform 0)
   (verbatim
     :accessor verbatim
     :initarg :verbatim
     :initform nil)))

(defmacro increment-column (column char)
  `(case ,char
     (#\Tab
       (incf ,column 8))
     (#\Newline
       (setf ,column 0))
     ((#\Backspace #\Page #\Return #\Rubout))
     (otherwise
       (incf ,column))))

(defun align-with-spaces (stream)
  (with-slots (input-column output-column output-stream) stream
    (do ()
        ((>= output-column input-column))
      (incf output-column)
      (write-char #\Space output-stream))))

(defmethod trivial-gray-streams:stream-write-char ((stream verbatim-stream) char)
  (increment-column (output-column stream) char)
  (align-with-spaces stream)
  (write-char char (output-stream stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream verbatim-stream))
  (finish-output (output-stream stream)))

(defmethod trivial-gray-streams:stream-listen ((stream verbatim-stream))
  (listen (input-stream stream)))

(defmethod trivial-gray-streams:stream-read-char ((stream verbatim-stream))
  (with-slots (input-stream output-stream input-column output-column verbatim) stream
    (or
      (when-let ((char (read-char input-stream nil)))
        (increment-column input-column char)
        (when (or verbatim (and (char/= #\Space char) (char/= #\Tab char)))
          (increment-column output-column char)
          (align-with-spaces stream)
          (write-char char output-stream))
        char)
      :eof)))

(defmethod trivial-gray-streams:stream-peek-char ((stream verbatim-stream))
  (peek-char nil (input-stream stream) nil :eof))

(defmethod trivial-gray-streams:stream-unread-char ((stream verbatim-stream) char)
  (unread-char (input-stream stream) char))

(defmethod trivial-gray-streams:stream-line-column ((stream verbatim-stream))
  (input-column stream))

(defmacro with-verbatim (stream &body body)
  (with-gensyms (verb st)
    `(let* ((,st ,stream)
            (,verb (verbatim ,st)))
       (unwind-protect
           (progn
             (setf (verbatim ,st) t)
             ,@body)
         (setf (verbatim ,st) ,verb)))))

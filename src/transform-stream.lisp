(in-package :cl-indentify)

(defclass transform-stream (trivial-gray-streams:fundamental-character-output-stream
                            trivial-gray-streams:fundamental-character-input-stream)
  ((input-stream
     :accessor input-stream
     :initarg :input-stream)
   (output-stream
     :accessor output-stream
     :initarg :output-stream)
   (column
     :accessor column
     :initarg :column
     :initform 0)
   (echo
     :accessor echo
     :initarg :echo
     :initform t)))

(defmethod trivial-gray-streams:stream-write-char ((stream transform-stream) char)
  (write-char char (output-stream stream)))

(defmethod trivial-gray-streams:stream-finish-output ((stream transform-stream))
  (finish-output (output-stream stream)))

(defmethod trivial-gray-streams:stream-listen ((stream transform-stream))
  (listen (input-stream stream)))

(defmethod trivial-gray-streams:stream-read-char ((stream transform-stream))
  (with-slots (input-stream output-stream column echo) stream
    (or
      (when-let ((ch (read-char input-stream nil)))
        (case ch
          (#\Tab
            (incf column 8))
          (#\Newline
            (setf column 0))
          ((#\Backspace #\Page #\Return #\Rubout))
          (otherwise
            (incf column)))
        (when echo
          (write-char ch output-stream))
        ch)
      :eof)))

(defmethod trivial-gray-streams:stream-peek-char ((stream transform-stream))
  (peek-char nil (input-stream stream) nil :eof))

(defmethod trivial-gray-streams:stream-unread-char ((stream transform-stream) char)
  (unread-char (input-stream stream) char))

(defmethod trivial-gray-streams:stream-line-column ((stream transform-stream))
  (column stream))

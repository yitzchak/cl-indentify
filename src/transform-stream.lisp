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
  (trivial-gray-streams:stream-write-char (output-stream stream) char))

(defmethod trivial-gray-streams:stream-finish-output ((stream transform-stream))
  (trivial-gray-streams:stream-finish-output (output-stream stream)))

(defmethod trivial-gray-streams:stream-listen ((stream transform-stream))
  (trivial-gray-streams:stream-listen (input-stream stream)))

(defmethod trivial-gray-streams:stream-read-char ((stream transform-stream))
  (with-slots (input-stream output-stream column echo) stream
    (when-let ((ch (trivial-gray-streams:stream-read-char input-stream stream)))
      (case ch
        (#\Tab
          (incf column 8))
        (#\Newline
          (setf column 0))
        ((#\Backspace #\Page #\Return #\Rubout))
        (otherwise
          (incf column)))
      (if echo
        (trivial-gray-streams:stream-write-char output-stream ch)
        ch))))

(defmethod trivial-gray-streams:stream-peek-char ((stream transform-stream))
  (trivial-gray-streams:stream-peek-char (input-stream stream)))

(defmethod trivial-gray-streams:stream-unread-char ((stream transform-stream) char)
  (trivial-gray-streams:stream-unread-char (input-stream stream) char))

(defmethod trivial-gray-streams:stream-line-column ((stream transform-stream))
  (column stream))

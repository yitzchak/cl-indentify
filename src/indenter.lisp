(in-package :cl-indentify)

(defclass indenter ()
  ((indent-templates
     :reader indent-templates
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

(defun number-token-p (token)
  (handler-case (numberp (read-from-string token nil))
    (reader-error ()
      nil)))

(defun indent-template (instance name)
  (unless (number-token-p name)
    (gethash (uiop:standard-case-symbol-name name)
             (indent-templates instance)
             '(:count 0))))

(defgeneric (setf indent-template) (value instance name))

(defmethod (setf indent-template) (value instance (name string))
  (setf (gethash (uiop:standard-case-symbol-name name) (indent-templates instance)) value))

(defun write-symbol-to-string (package-name symbol-name)
  (format nil "~A:~A" package-name symbol-name))

(defun symbol-names (sym)
  (if (keywordp sym)
    (list (prin1-to-string sym))
    (let ((pkg (symbol-package sym))
          (name (symbol-name sym)))
      (remove-duplicates
        (list*
          (prin1-to-string sym)
          name
          (write-symbol-to-string (package-name pkg) name)
          (mapcar
            (lambda (nickname)
              (write-symbol-to-string nickname name))
            (package-nicknames (symbol-package sym))))
        :test #'string=))))

(defun normalize-template (template)
  (let ((new-template (list :count (getf template :count 0))))
    (when-let ((ig (getf template :ignore)))
      (setf (getf new-template :ignore)
        (mapcan #'symbol-names ig)))
    (when-let ((primary (getf template :primary)))
      (setf (getf new-template :primary)
        (normalize-template primary)))
    (when-let ((secondary (getf template :secondary)))
      (setf (getf new-template :secondary)
        (normalize-template secondary)))
    new-template))

(defmethod (setf indent-template) (value instance (sym symbol))
  (with-slots (indent-templates) instance
    (dolist (name (symbol-names sym))
      (setf (gethash name indent-templates) value))))

(defun load-templates (instance &rest template-groups)
  (dolist (templates template-groups)
    (dolist (p templates)
      (setf (indent-template instance (car p)) (normalize-template (cdr p))))))

(defun load-default-templates (instance)
  (load-templates instance
                  +common-lisp-templates+
                  +asdf-templates+
                  +uiop-templates+
                  +alexandria-templates+))

(defun load-template-file (instance path)
  (with-open-file (indents-stream path :if-does-not-exist nil)
    (when indents-stream
      (do ((q (read indents-stream nil) (read indents-stream nil)))
          ((not q))
        (setf (indent-template instance (car q)) (cdr q))))))

(defun load-user-templates (instance)
  (load-template-file instance (uiop:xdg-config-home "cl-indentify" "templates.lisp")))

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

(defun scan-string (instance state quoted)
  (declare (ignore quoted))
  (scan-char instance state)
  (do ((ch (scan-char instance state) (scan-char instance state)))
      ((not ch))
    (case ch
      (#\\
        (scan-char instance state))
      (#\"
        (return '(:form))))))

(defun scan-indent (instance state quoted)
  (declare (ignore quoted))
  (with-slots (column output-stream) state
    (do ((ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
        ((not ch))
      (case ch
        ((#\Space #\Tab))
        (otherwise
          (unscan-char instance state ch)
          (return)))))
  '(:indent))

(defun scan-line-comment (instance state quoted)
  (declare (ignore quoted))
  (do ((ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
      ((not ch))
    (when (char= ch #\Newline)
      (unscan-char instance state ch)
      (return))
    (write-char ch (output-stream state)))
  '(:space))

(defun scan-token (instance state quoted)
  (declare (ignore quoted))
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

(defun scan-sharpsign-backslash (instance state quoted)
  (declare (ignore quoted))
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

(defun scan-sharpsign-asterisk (instance state quoted)
  (declare (ignore quoted))
  (do ((ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
      ((not ch))
    (case ch
      ((#\0 #\1))
      (otherwise
        (unscan-char instance state ch)
        (return))))
  '(:form))

(defun scan-sharpsign-rational (instance state quoted digits)
  (declare (ignore quoted))
  (do ((chars (concatenate 'string "/." digits))
       (ch (scan-char instance state :echo nil) (scan-char instance state :echo nil)))
      ((not ch))
    (unless (find ch chars :test #'char-equal)
      (unscan-char instance state ch)
      (return)))
  '(:form))

(defun scan-sharpsign-vertical-bar (instance state quoted)
  (declare (ignore quoted))
  (do* ((prev-ch nil ch)
        (ch (scan-char instance state) (scan-char instance state))
        (count 1))
       ((not ch))
    (cond
      ((not prev-ch))
      ((and (char= prev-ch #\#) (char= ch #\|))
        (incf count))
      ((and (char= prev-ch #\|) (char= ch #\#))
        (when (zerop (decf count))
          (return '(:space)))))))

(defun scan-sharpsign (instance state quoted)
  (do ((ch (scan-char instance state) (scan-char instance state)))
      ((not ch))
    (case ch
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
      (#\\
        (return (scan-sharpsign-backslash instance state quoted)))
      (#\'
        (return (scan-form instance state quoted)))
      (#\(
        (return (scan-forms instance state quoted)))
      (#\*
        (return (scan-sharpsign-asterisk instance state quoted)))
      (#\:
        (return (scan-token instance state quoted)))
      ((#\. #\c #\C #\a #\A #\s #\S #\p #\P #\n #\N #\+ #\-)
        (return (scan-form instance state quoted)))
      ((#\b #\B)
        (return (scan-sharpsign-rational instance state quoted "01")))
      ((#\o #\O)
        (return (scan-sharpsign-rational instance state quoted "01234567")))
      ((#\x #\X)
        (return (scan-sharpsign-rational instance state quoted "0123456789abcdef")))
      ((#\r #\R)
        (return (scan-sharpsign-rational instance state quoted "0123456789abcdefghijklmnopqrstuvwxyz")))
      (#\|
        (return (scan-sharpsign-vertical-bar instance state quoted)))
      (otherwise ; Either the sharpsign not followed by anything or we don't know what it is.
        (return '(:form))))))

(defun scan-chunk (instance state quoted &optional completed-form-count template)
  (with-slots (output-stream) state
    (when-let ((ch (scan-char instance state :echo nil)))
      (case ch
        (#\Newline
          (write-char ch (output-stream state))
          (scan-indent instance state quoted))
        (#\"
          (write-char ch (output-stream state))
          (scan-string instance state quoted))
        (#\;
          (write-char ch (output-stream state))
          (scan-line-comment instance state quoted))
        (#\#
          (write-char ch (output-stream state))
          (scan-sharpsign instance state quoted))
        ((#\Space #\Tab)
          (write-char ch (output-stream state))
          '(:space))
        (#\'
          (write-char ch (output-stream state))
          (scan-form instance state t))
        ((#\` #\@ #\,)
          (write-char ch (output-stream state))
          (scan-form instance state quoted))
        ((#\( #\[)
          (write-char ch (output-stream state))
          (scan-forms instance state quoted
                      (when template
                        (if (< completed-form-count (getf template :count))
                          (getf template :primary)
                          (getf template :secondary)))))
        ((#\) #\])
          (write-char ch (output-stream state))
          '(:exit))
        (otherwise
          (unscan-char instance state ch)
          (scan-token instance state quoted))))))

(defun scan-form (instance state quoted)
  (with-slots (output-stream) state
    (do ((chunk (scan-chunk instance state quoted) (scan-chunk instance state quoted)))
        ((not chunk))
      (unless (eql (car chunk) :space)
        (return chunk)))))

(defun scan-forms (instance state quoted &optional template)
  (with-slots (column output-stream) state
    (do* ((indent column)
          (primary-indent (+ 3 indent))
          (secondary-indent (1+ indent))
          (completed-form-count -1)
          (previous-column column column)
          (form (scan-chunk instance state quoted completed-form-count template)
                (scan-chunk instance state quoted completed-form-count template)))
         ((not form))
      (case (car form)
        (:indent
          (setf column
            (cond
              ((not template) indent)
              ((>= completed-form-count (getf template :count))
                secondary-indent)
              (t primary-indent)))
          (dotimes (k column)
            (write-char #\Space output-stream)))
        (:form
          (incf completed-form-count)
          (cond
            ((and (not quoted) (not template) (zerop completed-form-count) (cadr form))
              (setf template (indent-template instance (cadr form))))
            ((and template (not (zerop (getf template :count))) (= 1 completed-form-count))
              (setf primary-indent previous-column))
            ((and template (= (1+ (getf template :count)) completed-form-count))
              (setf secondary-indent previous-column)))
          (when (and template
                     (member (uiop:standard-case-symbol-name (cadr form)) (getf template :ignore)
                             :test #'string=)
                     (<= completed-form-count (getf template :count)))
            (decf completed-form-count)))
        (:exit
          (return '(:form)))))))

(defun indentify (instance &optional input-stream output-stream)
  (let ((state (make-instance 'indenter-state
                              :input-stream (or input-stream *standard-input*)
                              :output-stream (or output-stream *standard-output*))))
    (scan-indent instance state nil)
    (dotimes (k (column state))
      (write-char #\Space output-stream))
    (scan-forms instance state nil)))


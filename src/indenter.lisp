(in-package :cl-indentify)

(defparameter *indent-templates* (make-hash-table :test 'equalp))

(defun number-token-p (token)
  (handler-case (numberp (read-from-string token nil))
    (reader-error ()
      nil)))

(defun indent-template (name)
  (unless (number-token-p name)
    (gethash (uiop:standard-case-symbol-name name)
             *indent-templates*
             '(:count 0))))

(defgeneric (setf indent-template) (value name))

(defmethod (setf indent-template) (value (name string))
  (setf (gethash (uiop:standard-case-symbol-name name) *indent-templates*)
    (normalize-template value)))

(defun write-symbol-to-string (package-name symbol-name &key (exported t))
  (concatenate 'string
               package-name
               (if exported ":" "::")
               symbol-name))

(defun symbol-names (sym)
  (if (keywordp sym)
    (list (prin1-to-string sym))
    (remove-duplicates
      (let ((name (symbol-name sym)))
        (mapcan
          (lambda (pkg)
            (multiple-value-bind (other-sym status) (find-symbol name pkg)
              (when (and (eql status :external) (eql other-sym sym))
                (list*
                  (prin1-to-string sym)
                  name
                  (write-symbol-to-string (package-name pkg) name)
                  (mapcar
                    (lambda (nickname)
                      (write-symbol-to-string nickname name))
                    (package-nicknames pkg))))))
          (list-all-packages)))
      :test #'string=)))

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

(defmethod (setf indent-template) (value (sym symbol))
  (let ((normalized-template (normalize-template value)))
    (dolist (name (symbol-names sym))
      (setf (gethash name *indent-templates*) normalized-template))))

(defun load-templates (&rest template-groups)
  (dolist (templates template-groups)
    (dolist (p templates)
      (setf (indent-template (car p)) (cdr p)))))

(defun load-default-templates ()
  (load-templates +common-lisp-templates+
                  +asdf-templates+
                  +uiop-templates+
                  +alexandria-templates+))

(defun load-template-file (path)
  (with-open-file (indents-stream path :if-does-not-exist nil)
    (when indents-stream
      (do ((q (read indents-stream nil) (read indents-stream nil)))
          ((not q))
        (setf (indent-template (car q)) (cdr q))))))

(defun load-user-templates ()
  (load-template-file (uiop:xdg-config-home "cl-indentify" "templates.lisp")))

(defun scan-string (stream &optional template)
  (declare (ignore template))
  (read-char stream nil)
  (do ((ch (read-char stream nil) (read-char stream nil)))
      ((not ch))
    (case ch
      (#\\
        (read-char stream nil))
      (#\"
        (return '(:form))))))

(defun scan-indent (stream &optional template)
  (declare (ignore template))
  (without-echo stream
    (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
        ((or (not ch)
             (and (char/= ch #\Space)
                  (char/= ch #\Tab)))
                  '(:indent))
      (read-char stream nil))))

(defun scan-line-comment (stream &optional template)
  (declare (ignore template))
  (read-char stream nil)
  (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((or (not ch) (char= ch #\Newline)))
    (read-char stream nil))
  '(:space))

(defun scan-token (stream &optional template)
  (declare (ignore template))
  (list
    :form
    (with-output-to-string (token-stream)
      (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
          ((not ch))
        (case ch
          ((#\Space #\Tab #\Newline #\( #\) #\' #\` #\, #\@ #\;)
            (return))
          (otherwise
            (read-char stream nil)
            (write-char ch token-stream)))))))

(defun scan-sharpsign-backslash (stream &optional template)
  (declare (ignore template))
  (do ((pos 0 (1+ pos))
       (ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((not ch))
    (case ch
      ((#\Space #\Newline)
        (return))
      ((#\( #\) #\' #\` #\, #\@ #\;)
        (when (zerop pos)
          (read-char stream nil))
        (return))
      (otherwise
        (read-char stream nil))))
  '(:form))

(defun scan-sharpsign-asterisk (stream &optional template)
  (declare (ignore template))
  (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((not ch))
    (case ch
      ((#\0 #\1)
        (read-char stream nil))
      (otherwise
        (return))))
  '(:form))

(defun scan-sharpsign-rational (digits stream &optional template)
  (declare (ignore template))
  (do ((chars (concatenate 'string "/." digits))
       (ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((not ch))
    (if (find ch chars :test #'char-equal)
      (read-char stream nil)
      (return)))
  '(:form))

(defun scan-sharpsign-vertical-bar (stream &optional template)
  (declare (ignore template))
  (do* ((prev-ch nil ch)
        (ch (read-char stream nil) (read-char stream nil))
        (count 1))
       ((not ch))
    (cond
      ((not prev-ch))
      ((and (char= prev-ch #\#) (char= ch #\|))
        (incf count))
      ((and (char= prev-ch #\|) (char= ch #\#))
        (when (zerop (decf count))
          (return '(:space)))))))

(defun scan-sharpsign (stream &optional template)
  (read-char stream nil)
  (do ((ch (read-char stream nil) (read-char stream nil)))
      ((not ch))
    (case ch
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
      (#\\
        (return (scan-sharpsign-backslash stream template)))
      (#\'
        (return (scan-form stream template)))
      (#\(
        (return (scan-forms stream template)))
      (#\*
        (return (scan-sharpsign-asterisk stream template)))
      (#\:
        (return (scan-token stream template)))
      ((#\. #\c #\C #\a #\A #\s #\S #\p #\P #\n #\N #\+ #\-)
        (return (scan-form stream template)))
      ((#\b #\B)
        (return (scan-sharpsign-rational "01" stream template)))
      ((#\o #\O)
        (return (scan-sharpsign-rational "01234567" stream template)))
      ((#\x #\X)
        (return (scan-sharpsign-rational "0123456789abcdef" stream template)))
      ((#\r #\R)
        (return (scan-sharpsign-rational "0123456789abcdefghijklmnopqrstuvwxyz" stream template)))
      (#\|
        (return (scan-sharpsign-vertical-bar stream template)))
      (otherwise ; Either the sharpsign not followed by anything or we don't know what it is.
        (return '(:form))))))

(defun scan-chunk (stream &optional template)
  (when-let ((ch (peek-char nil stream nil)))
    (case ch
      (#\Newline
        (read-char stream nil)
        (scan-indent stream template))
      (#\"
        (scan-string stream template))
      (#\;
        (scan-line-comment stream template))
      (#\#
        (scan-sharpsign stream template))
      ((#\Space #\Tab)
        (read-char stream nil)
        '(:space))
      (#\'
        (read-char stream nil)
        (scan-form stream '(:quoted t)))
      ((#\` #\@ #\,)
        (read-char stream nil)
        (scan-form stream template))
      ((#\( #\[)
        (read-char stream nil)
        (scan-forms stream template))
      ((#\) #\])
        (read-char stream nil)
        '(:exit))
      (otherwise
        (scan-token stream template)))))

(defun scan-form (stream &optional template)
  (do ((chunk (scan-chunk stream template) (scan-chunk stream template)))
      ((not chunk))
    (unless (eql (car chunk) :space)
      (return chunk))))

(defun select-template (template completed-form-count)
  (cond
    ((or (not template) (getf template :quoted))
      template)
    ((< completed-form-count (getf template :count 0))
      (getf template :primary))
    (t
      (getf template :secondary))))

(defun write-indent (stream)
  (dotimes (k (column stream))
    (write-char #\Space stream)))

(defun scan-forms (stream &optional template)
  (with-slots (column) stream
    (do* ((indent column)
          (primary-indent (+ 3 indent))
          (secondary-indent (1+ indent))
          (completed-form-count -1)
          (previous-column column column)
          (form (scan-chunk stream (select-template template completed-form-count))
                (scan-chunk stream (select-template template completed-form-count))))
         ((not form))
      (case (car form)
        (:indent
          (setf column
            (cond
              ((or (not template) (getf template :quoted))
                indent)
              ((>= completed-form-count (getf template :count 0))
                secondary-indent)
              (t primary-indent)))
          (write-indent stream))
        (:form
          (incf completed-form-count)
          (cond
            ((and (not template) (not (getf template :quoted)) (zerop completed-form-count) (cadr form))
              (setf template (indent-template (cadr form))))
            ((and template (not (zerop (getf template :count 0))) (= 1 completed-form-count))
              (setf primary-indent previous-column))
            ((and template (= (1+ (getf template :count 0)) completed-form-count))
              (setf secondary-indent previous-column)))
          (when (and template
                     (member (uiop:standard-case-symbol-name (cadr form)) (getf template :ignore)
                             :test #'string=)
                     (<= completed-form-count (getf template :count 0)))
            (decf completed-form-count)))
        (:exit
          (return '(:form)))))))

(defun indentify (&optional input-stream output-stream)
  (let ((stream (make-instance 'synchronized-stream
                               :input-stream (or input-stream *standard-input*)
                               :output-stream (or output-stream *standard-output*))))
    (scan-indent stream)
    (write-indent stream)
    (scan-forms stream)))


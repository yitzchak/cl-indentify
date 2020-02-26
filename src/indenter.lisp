(in-package :cl-indentify)

(defclass indenter ()
  ((indent-templates
     :reader indent-templates
     :initform (make-hash-table :test 'equalp))
   (tab-size
     :accessor tab-size
     :initform 8
     :initarg :tab-size)))

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
  (setf (gethash (uiop:standard-case-symbol-name name) (indent-templates instance))
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

(defmethod (setf indent-template) (value instance (sym symbol))
  (with-slots (indent-templates) instance
    (let ((normalized-template (normalize-template value)))
      (dolist (name (symbol-names sym))
        (setf (gethash name indent-templates) normalized-template)))))

(defun load-templates (instance &rest template-groups)
  (dolist (templates template-groups)
    (dolist (p templates)
      (setf (indent-template instance (car p)) (cdr p)))))

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

(defun scan-string (instance stream quoted)
  (declare (ignore instance quoted))
  (read-char stream nil)
  (do ((ch (read-char stream nil) (read-char stream nil)))
      ((not ch))
    (case ch
      (#\\
        (read-char stream nil))
      (#\"
        (return '(:form))))))

(defun scan-indent (instance stream quoted)
  (declare (ignore instance quoted))
  (setf (echo stream) nil)
  (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((or (not ch)
           (and (char/= ch #\Space)
                (char/= ch #\Tab))))
    (read-char stream nil))
  (setf (echo stream) t)
  '(:indent))

(defun scan-line-comment (instance stream quoted)
  (declare (ignore instance quoted))
  (read-char stream nil)
  (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((or (not ch) (char= ch #\Newline)))
    (read-char stream nil))
  '(:space))

(defun scan-token (instance stream quoted)
  (declare (ignore instance quoted))
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

(defun scan-sharpsign-backslash (instance stream quoted)
  (declare (ignore instance quoted))
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

(defun scan-sharpsign-asterisk (instance stream quoted)
  (declare (ignore instance quoted))
  (do ((ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((not ch))
    (case ch
      ((#\0 #\1)
        (read-char stream nil))
      (otherwise
        (return))))
  '(:form))

(defun scan-sharpsign-rational (instance stream quoted digits)
  (declare (ignore instance quoted))
  (do ((chars (concatenate 'string "/." digits))
       (ch (peek-char nil stream nil) (peek-char nil stream nil)))
      ((not ch))
    (if (find ch chars :test #'char-equal)
      (read-char stream nil)
      (return)))
  '(:form))

(defun scan-sharpsign-vertical-bar (instance stream quoted)
  (declare (ignore instance quoted))
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

(defun scan-sharpsign (instance stream quoted)
  (read-char stream nil)
  (do ((ch (read-char stream nil) (read-char stream nil)))
      ((not ch))
    (case ch
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
      (#\\
        (scan-sharpsign-backslash instance stream quoted))
      (#\'
        (return (scan-form instance stream quoted)))
      (#\(
        (return (scan-forms instance stream quoted)))
      (#\*
        (return (scan-sharpsign-asterisk instance stream quoted)))
      (#\:
        (return (scan-token instance stream quoted)))
      ((#\. #\c #\C #\a #\A #\s #\S #\p #\P #\n #\N #\+ #\-)
        (return (scan-form instance stream quoted)))
      ((#\b #\B)
        (return (scan-sharpsign-rational instance stream quoted "01")))
      ((#\o #\O)
        (return (scan-sharpsign-rational instance stream quoted "01234567")))
      ((#\x #\X)
        (return (scan-sharpsign-rational instance stream quoted "0123456789abcdef")))
      ((#\r #\R)
        (return (scan-sharpsign-rational instance stream quoted "0123456789abcdefghijklmnopqrstuvwxyz")))
      (#\|
        (return (scan-sharpsign-vertical-bar instance stream quoted)))
      (otherwise ; Either the sharpsign not followed by anything or we don't know what it is.
        (return '(:form))))))

(defun scan-chunk (instance stream quoted &optional completed-form-count template)
  (when-let ((ch (peek-char nil stream nil)))
    (case ch
      (#\Newline
        (read-char stream nil)
        (scan-indent instance stream quoted))
      (#\"
        (scan-string instance stream quoted))
      (#\;
        (scan-line-comment instance stream quoted))
      (#\#
        (scan-sharpsign instance stream quoted))
      ((#\Space #\Tab)
        (read-char stream nil)
        '(:space))
      (#\'
        (read-char stream nil)
        (scan-form instance stream t))
      ((#\` #\@ #\,)
        (read-char stream nil)
        (scan-form instance stream quoted))
      ((#\( #\[)
        (read-char stream nil)
        (scan-forms instance stream quoted
                    (when template
                      (if (< completed-form-count (getf template :count))
                        (getf template :primary)
                        (getf template :secondary)))))
      ((#\) #\])
        (read-char stream nil)
        '(:exit))
      (otherwise
        (scan-token instance stream quoted)))))

(defun scan-form (instance stream quoted)
  (do ((chunk (scan-chunk instance stream quoted) (scan-chunk instance stream quoted)))
      ((not chunk))
    (unless (eql (car chunk) :space)
      (return chunk))))

(defun scan-forms (instance stream quoted &optional template)
  (with-slots (column) stream
    (do* ((indent column)
          (primary-indent (+ 3 indent))
          (secondary-indent (1+ indent))
          (completed-form-count -1)
          (previous-column column column)
          (form (scan-chunk instance stream quoted completed-form-count template)
                (scan-chunk instance stream quoted completed-form-count template)))
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
            (write-char #\Space stream)))
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
  (let ((stream (make-instance 'transform-stream
                              :input-stream (or input-stream *standard-input*)
                              :output-stream (or output-stream *standard-output*))))
    (scan-indent instance stream nil)
    (dotimes (k (column stream))
      (write-char #\Space stream))
    (scan-forms instance stream nil)))


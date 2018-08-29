(in-package #:zoglog)

;; Maximum log entries on one page
(defvar *log-display-limit* 1000)

(setf cl-who:*attribute-quote-char* #\")
(setf (cl-who:html-mode) :HTML5)
(setf cl-who:*escape-char-p* #'cl-who::minimal-plus-quotes-escape-char-p)

(defmacro deftemplate (name &body body)
  "Create template function that returns blocks for base template."
  `(defun ,name (&optional args)
     (list
      ,@(loop for form in body
           append `(,(values (intern (symbol-name (car form)) "KEYWORD"))
                     (cl-who:with-html-output-to-string (s nil :indent t)
                       ,@(cdr form)))))))

(defun render-template (template-name &optional args)
  "Render base template with blocks from TEMPLATE-NAME."
  (let ((blocks (funcall template-name args)))
    (base-tpl blocks args)))

(defmacro with-args ((&rest vars) args-list &body body)
  "Make LET block with variables defined from args plist."
  `(let (,@(loop for var in vars
              collect
                `(,var (getf ,args-list
                             (intern (symbol-name ',var) "KEYWORD")))))
     ,@body))

(defmacro select-options (list condition)
  "Create list of option tags with selected value."
  (let ((item (gensym))
        (selected (gensym)))
    `(loop for ,item in ,list
        with ,selected = ,condition
        do (cl-who:htm (:option
                        :value ,item
                        :selected (equal ,item ,selected)
                        (cl-who:str ,item))))))

;; Very simple URL regex
(defparameter +url-regex+ (cl-ppcre:create-scanner
                           "\\b(((ftp|http)s?|file)://[^\\s]+)"))

(defun replace-with-link (target-string
                          start
                          end
                          match-start
                          match-end
                          reg-starts
                          reg-ends)
  "Replace link with html. TODO: url-encode href properly."
  (declare (ignore start end reg-starts reg-ends))
  (format nil "<a rel=\"nofollow\" href=\"~a\">~:*~a</a>"
          (subseq target-string
                  match-start
                  match-end)))

(defun linkify (str)
  (cl-ppcre:regex-replace-all +url-regex+
                              (cl-who:escape-string str)
                              #'replace-with-link))


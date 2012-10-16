;;; rosemacs-repl.el --- Show ROS relates stats in line above REPL
;;;
;;;
;;;  The key idea is that the REPL can become some sort fo dashboard,
;;;  showing the current status of the robot.
;;;
;;;  uses: http://www.emacswiki.org/emacs/xml-rpc.el
;;;
;;;

(require 'xml-rpc)
(require 'rosemacs)

;; the var contains user defined plugins
(defvar *rosemacs-repl-plugins* (list 'roscore-running 'rosnode-status))
;; describes the prepromt, between last result and prompt, e.g. before CL-USER>
(defvar *rosemacs-repl-prepromt* "\n%s\n")
(defvar *rosemacs-repl-separator* " ")

(defvar *rosemacs-repl-mode* nil)

(defun get-masteruri-patched ()
  "returns master uri replacing localhost with hostname"
  ;; localhost is bugged on my machine
  (let ((m-uri (getenv "ROS_MASTER_URI")))
    (unless (null m-uri)
      (replace-regexp-in-string "\\(localhost\\)" "127.0.0.1" m-uri))))

(defun rosemacs-preprompt ()
  "creates the prompt to be dsiplayed before REPL prompt"
  (let ((output ""))
    (loop for plugin in *rosemacs-repl-plugins* do
      (let ((funcall-result (funcall plugin)))
        (unless (null funcall-result)
          (setf output (concatenate 'string output (funcall plugin) *rosemacs-repl-separator*)))))
    (if (< 0 (length output))
       (format *rosemacs-repl-prepromt* output)
        ;; (concatenate 'string "\n" output "\n")
        "")))

(defadvice slime-repl-insert-prompt (before sr-ip activate)
  "called before insert prompt to first insert custom state info"
  (when *rosemacs-repl-mode*
    (with-current-buffer (slime-output-buffer)
      (insert-before-markers (rosemacs-preprompt))
      (goto-char slime-repl-input-start-mark))))

(defun rosemacs-repl-mode (&optional enable)
  "inverst mode without argument, set mode else"
  (interactive)
  (setf *rosemacs-repl-mode* (or enable (not *rosemacs-repl-mode*)))
  (if *rosemacs-repl-mode*
      (message "rosemacs-repl-mode enabled")
    (message "rosemacs-repl-mode disabled")))



;; URL debug msgs very annoying, filtering it out
(defadvice url-debug (around rosemacs-url-debug activate)
;; kills (url-http-debug "Contacting host: %s:%d" host 11311)
  (if (not *rosemacs-repl-mode*)
      ad-do-it
      (unless
          (and
           ;; careful not to cause error in string-match
           (equal tag 'http)
           (not (null args))
           (typep args 'cons)
           (typep (car args) 'string)
           (typep (cdr args) 'cons)
           (typep (cadr args) 'string)
           (not (null (string-match "Contacting host: %s:%d" (car args))))
           (typep (cddr args) 'cons)
           (typep (caddr args) 'integer)
           (=  11311 (caddr args)))
        ad-do-it)))

(defadvice url-lazy-message (around rosemacs-url-lazy-message activate)
;; kills (url-lazy-message "Contacting host: %s:%d" host 11311)
  (if (not *rosemacs-repl-mode*)
      ad-do-it
      (unless
          (and
           ;; careful not to cause error in string match
           (not (null args))
           (typep args 'cons)
           (typep (car args) 'string)
           (or
            (not (null (string-match "Contacting host: %s:%d" (car args))))
            (not (null (string-match "Reading %s..." (car args))))))
        ad-do-it)))


(defun call-ros-master (&rest args)
  "calls xmlrpc where args is the method name plus arguments"
  (condition-case nil
      (apply 'xml-rpc-method-call
       (get-masteruri-patched)
       (nconc (copy-list args) (list "rosemacs-repl")))
    (error nil)))

(defun slime-var-value-string (package-name var-symbol)
  "looks up variable symbol in slime, evals and return result of eval as string"
;; example: (slime-var-value-string "roslisp" "*node-status*")
  (let ((result
          (slime-eval
           ;; swank takes a string to eval and returns a string
           `(swank:eval-and-grab-output
             ,(format
       "(cl:eval
        (cl:find-if
         (cl:lambda (x)
                    (cl:string= (cl:string-upcase \"%s\")
                                (cl:package-name (cl:symbol-package x))))
         (cl:find-all-symbols (cl:copy-symbol '%s))))"
       package-name var-symbol
       )))))
    (when result (second result))))

(defun slime-var-value (package-name var-symbol)
  "reads variable value in slime, then tries to read result"
  (condition-case nil
      (eval (read (downcase
                   (slime-var-value-string package-name var-symbol))))
    (error nil)))

;; plugins
(defun roscore-running ()
  "returns ros master URI when found, else ---"
  (let ((result (or
                 (slime-var-value "roslisp" "*master-uri*")
                 (third (call-ros-master "getUri")))))
    (or result
      "---")))

(defun rosnode-status ()
  "return value of roslisp::*node-status*"
  (let ((status (slime-var-value "roslisp" "*node-status*")))
    (cond
      ((not (null status))
       (symbol-name status))
      (t "--- "))))

;; ELISP> (xml-rpc-method-call  "http://krulap:11311/" 'getUri "rosemacs")
;; (1 nil "http://krulap:11311/")
;; ELISP> (xml-rpc-method-call  "http://localhost:11311/" 'getUri "rosemacs")
;; *** Eval error ***  Why? url-http-response-status is nil




(provide 'rosemacs-repl)

;;; end rosemacs-repl.el
;;; adapto-util.el --- keyboard shortcuts for the adapto project

;; adds hooks into slime so that after slime starts,
;; certain ros packages are always loaded, and a
;; given lisp package is chosen

;; Use in your emacs init with (examples):
;; (require 'adapto-util)
;; (push '("adapto" ;; project name
;;         "ad-c-exe" ;; initial lisp package
;;         (("adapto_executive" "adapto-executive")))
;;       *adapto-startup-projects*)
;; (push '("turtle"
;;         "turtle-hl"
;;         (("turtle_process_modules" "turtle-process-modules")))
;;       *adapto-startup-projects*)
;;
;; Also useful:
;;
;;(global-set-key "\C-cl" '(lambda ()
;;                           (interactive)
;;                           (slime-ros)
;;                           (rosemacs-repl-mode t)))
;; (global-set-key "\C-cr"
;;                 '(lambda ()
;;                   (interactive)
;;                   (slime-restart-inferior-lisp)))
;; (global-set-key "\C-cf"
;;                 '(lambda ()
;;                   (interactive)
;;                   (slime-quit-lisp)))
;;
;;

(require 'rosemacs)

;; List of project names, start packages and ros systems to load for each
;; systems are pairs of ros-pkg and asd system to load
(defvar *adapto-startup-projects* nil)

;; build up string from constants
(defun get-adapto-startup-command ()
  "returns a string containing lisp commands for the repl, like ros-load-system"
  (unless (null *adapto-startup-projects*)
    ;; load first project if only one exists, else ask user
    (let* ((project-name (if (= 1 (length *adapto-startup-projects*))
                             (caar *adapto-startup-projects*)
                           (ido-completing-read
                            "select project: "
                            (cons "none" (mapcar 'car *adapto-startup-projects*))
                            nil 1 nil nil nil)))
           (start-package (cadar (member-if
                                  '(lambda (x) (string= project-name (car x)))
                                  *adapto-startup-projects*)))
           (load-list (caddar (member-if
                               '(lambda (x) (string= project-name (car x)))
                               *adapto-startup-projects*)))
           (result ""))
      (dolist (system-pair (reverse load-list))
        (setf result
              (concatenate 'string
                           result
                           "(ros-load:load-system \"" (first system-pair) "\" \""
                           (second system-pair) "\") ")))
      (unless (null start-package)
        (setf result
              (concatenate 'string result "(in-package " start-package ")")))
      (unless (string= result "")
        (setf result
              (concatenate 'string result "(values)"))
        result))))


(defcustom slime-adapto-connected-hook nil
  "List of functions to call when SLIME connects to Lisp ADAPTO image."
  :type 'hook
  :group 'slime-lisp)

(add-hook 'slime-connected-hook (lambda ()
     (run-hooks 'slime-adapto-connected-hook)))

(defun adapto-load-project-or-system ()
  (interactive)
  (when (slime-connected-p)
    (condition-case nil
        (let ((slime-cmd (get-adapto-startup-command)))
          (unless (null slime-cmd)
            (slime-repl-eval-string slime-cmd)))
      (error nil))))

;; slime eval after slime has come up
(add-hook 'slime-adapto-connected-hook
          'adapto-load-project-or-system)


(provide 'adapto-util)

;;; end adapto-util.el

(setq inhibit-startup-message t)

; solve problems with copy and paste (hopefully)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(defun smooth-scroll (increment)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.06)
  (scroll-up increment))

(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))


; Public emacs site
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-color-themes")
(add-to-list 'load-path "~/.emacs.d/site")


(cua-mode t)
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour


;-*-Lisp-*-
;; prevent C-f C-l (needs to be before load slime)
(setq slime-bind-keys nil)


;; function to call sbcl and slime
(defun sbcl ()
  "Inferior SBCL"
  (interactive)
  (setq slime-rpl-used? nil)
  (let ( (inferior-lisp-program "/usr/bin/sbcl") )
    (slime)))

; Use those for loading and killing kibo
(global-set-key "\C-cl" 'acl-rpl)
(global-set-key "\C-cf"
                '(lambda ()
                  (interactive)
                  (slime-quit-lisp)))

;; slime (might prevent ROS)
;;(if (file-readable-p "/usr/local/lehrstuhl/DIR/lisp/config-host/slime")
;;(load "/usr/local/lehrstuhl/DIR/lisp/config-host/slime"))
;; slime from git
;; (if (file-readable-p "/usr/wiss/kargm/work/lisp/slime")
;;    (load "/usr/wiss/kargm/work/lisp/slime"))

(tool-bar-mode -1)

; other settings
(require 'paren)
(show-paren-mode t)
(global-set-key '[delete] 'delete-char)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(bar-cursor-mode t nil (bar-cursor))
 '(c-basic-offset (quote set-from-style))
 '(column-number-mode t)
 '(cua-keep-region-after-copy nil)
 '(delete-selection-mode t)
 '(frame-background-mode (quote light))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-case-fold t)
 '(ido-confirm-unique-completion nil)
 '(ido-create-new-buffer (quote alway))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-work-file-list 30)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/.ido.last")
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(ispell-local-dictionary "american" t)
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(mark-diary-entries-in-calendar t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-scroll-amount (quote (2 . 1)))
 '(next-line-add-newlines nil)
 '(paren-mode (quote paren) nil (paren))
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(ros-completion-function (quote ido-completing-read))
 '(safe-local-variable-values (quote ((Syntax . Common-Lisp) (Package . SAX) (Encoding . utf-8) (Syntax . COMMON-LISP) (Package . CL-PPCRE) (package . rune-dom) (readtable . runes) (Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(savehist-mode t nil (savehist))
 '(scroll-bar-mode (quote right))
 '(slime-backend "/home/kargm/work/lisp/slime/swank-loader.lisp")
 '(slime-complete-symbol*-fancy t)
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(slime-ros-completion-function (quote ido-completing-read))
 '(transient-mark-mode t)
 '(view-diary-entries-initially t))

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode"   t)
(autoload 'objc-mode "cc-mode" "Objective C Editing Mode" t)
(autoload 'text-mode "indented-text-mode" "Indented Text Editing Mode" t)
(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
(autoload 'ps-mode "ps-mode" "Major mode for editing PostScript" t)
(setq auto-mode-alist
  (append '(("\\.C$"       . c++-mode)
      ("\\.cc$"      . c++-mode)
      ("\\.c$"       . c-mode)
      ("\\.h$"       . c++-mode)
      ("\\.i$"       . c++-mode)
      ("\\.ii$"      . c++-mode)
      ("\\.m$"       . objc-mode)
      ("\\.pl$"      . perl-mode)
      ("\\.sql$"     . c-mode)
      ("\\.sh$"      . shell-script-mode)
      ("\\.mak$"     . makefile-mode)
      ("\\.GNU$"     . makefile-mode)
      ("makefile$"   . makefile-mode)
      ("Makefile$"   . makefile-mode)
      ("Imakefile$"  . makefile-mode)
      ("\\.Xdefaults$"    . xrdb-mode)
      ("\\.Xenvironment$" . xrdb-mode)
      ("\\.Xresources$"   . xrdb-mode)
      ("*.\\.ad$"         . xrdb-mode)
      ("\\.[eE]?[pP][sS]$" . ps-mode)
      ("\\.nsp"      . lisp-mode)
      ("\\.asd"      . lisp-mode)
      ("\\.vimpulse" . lisp-mode)
      ("\\.cl$"      . lisp-mode)
      ("\\.urdf$" . xml-mode)
      ) auto-mode-alist))

(setq default-tab-width 2)
(setq tab-width 2)
(setq-default c-basic-offset 2)
(setq-default indent-tabs-mode nil) 

(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(global-font-lock-mode t)               ;colorize all buffers
(setq font-lock-maximum-decoration t)

(setq-default indent-tabs-mode nil)

; Search highlighting
; highlight during query
(setq query-replace-highlight t)
; highlight incremental search
(setq search-highlight t)

(add-hook 'slime-mode-hook
  (lambda ()
    ;;; adjust lisp indentation
    (set-variable lisp-indent-function 'common-lisp-indent-function)
    (put 'if 'common-lisp-indent-function '(2 &rest 2))
    (put 'cond 'common-lisp-indent-function '(&rest (&whole 2 &rest 2)))
    (put 'let  'common-lisp-indent-function '((&whole 4 &rest (&whole 2 1 2)) &body))
    (put 'let* 'common-lisp-indent-function '((&whole 4 &rest (&whole 2 1 2)) &body))
    (put 'defclass 'common-lisp-indent-function '(6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
    (put 'make-instance 'common-lisp-indent-function '(4 &rest 2))


    (define-key slime-mode-map "\r" 'newline-and-indent)
    (define-key slime-mode-map [tab] 'slime-fuzzy-indent-and-complete-symbol)
    ))


;; (add-to-list 'load-path "/usr/share/common-lisp/source/slime/contrib")
;; (add-to-list 'load-path "/usr/share/common-lisp/source/slime/")
(add-to-list 'load-path "/home/kargm/work/lisp/slime")
(require 'slime)


;;something nasty undoes this change when just in customize options
(setf slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))

(defun kibobot ()
  (interactive)
  (setq *rpl-cmd-string* "(kibo) (in-package :kibo) (values)")
;  (let ((*rpl-cmd-string* "(kibo-ccrl) (in-package :kibo)"))
  (acl-rpl))

; paredit mode
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'inferior-lisp-mode-hook (lambda () (paredit-mode +1)))

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(define-key paredit-mode-map (kbd "C-<right>") 'forward-word)
(define-key paredit-mode-map (kbd "C-<left>") 'backward-word)
(define-key paredit-mode-map (kbd "<delete>") 'delete-char)

(add-hook 'emacs-lisp-mode-hook (lambda () (light-symbol-mode)))
(add-hook 'lisp-mode-hook (lambda () (light-symbol-mode)))
(add-hook 'python-mode-hook (lambda () (light-symbol-mode)))
(add-hook 'shell-script-mode-hook (lambda () (light-symbol-mode)))
(add-hook 'c-mode-hook (lambda () (light-symbol-mode)))
(add-hook 'c++-mode-hook (lambda () (light-symbol-mode)))
(autoload 'light-symbol-mode "light symbol mode"
  "highlight code."
  t)

(load "light-symbol-mode.el")

; beep is just a visual
(setq visible-bell 1)

; Ignore .svn stuff in grep-find
(setq grep-find-command "find . -type f -not -name \"*.svn-base\" -and -not -name \"*.tmp\" -print0 | xargs -0 -e grep -i -n -s -F ")

; all buffers have tba like buttons showing other buffers
(require 'tabbar)
(tabbar-mode)

; will allow you to type just "y" instead of "yes" when you exit.
(fset 'yes-or-no-p 'y-or-n-p)

; make parens a different color
(require 'parenface)

; use firefox
(setq browse-url-browser-function 'browse-url-firefox)


;; hl-line: highlight the current line
;; (when (fboundp 'global-hl-line-mode)
;;   (global-hl-line-mode t))
;; turn it on for all modes by default

(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "gray15")

;; restore window configuration
(require 'winner)
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; use super + arrow keys to switch between visible buffers
(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super) ;using windows key
  ;;  (windmove-default-keybindings)
  )

;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 15)
(setq recentf-exclude (append recentf-exclude '(".ftp:.*" ".sudo:.*")))
(setq recentf-keep '(file-remote-p file-readable-p))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; For a much better buffer list:
(global-set-key "\C-x\C-b" 'electric-buffer-list)

;;smex replaces M-x with IDO exhancements
(setq smex-save-file "~/.emacs.d/smex.save")
(require 'smex)
(smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(slime-repl-output-face ((t (:inherit font-lock-string-face :foreground "white")))))

(add-to-list 'load-path "/opt/ros/electric/ros/tools/rosemacs")
(require 'rosemacs)
(invoke-rosemacs)

(global-set-key "\C-x\C-r" ros-keymap)

(require 'rng-loc)
(condition-case nil ;; error e.g. when running as root
    (push (concat (ros-package-path "rosemacs") "/rng-schemas.xml") rng-schema-locating-files)
  (error nil))
(add-to-list 'auto-mode-alist '("\.launch$" . nxml-mode))
(add-to-list 'auto-mode-alist '("manifest.xml" . nxml-mode))


(slime-setup '(slime-fancy slime-asdf slime-ros))


;; Thibaults rosemacs plugin
(require 'rosemacs-repl)

;; adapto project
; Use those for loading and killing kibo
(require 'adapto-util)
(global-set-key "\C-cl" '(lambda ()
                          (interactive)
                          (slime-ros)
                          (rosemacs-repl-mode t)))
(global-set-key "\C-cr"
                '(lambda ()
                  (interactive)
                  (slime-restart-inferior-lisp)))
(global-set-key "\C-cf"
                '(lambda ()
                  (interactive)
                  (slime-quit-lisp)))

(push '("adapto"
        "ad-exe"
        (("adapto_executive" "adapto-executive")))
      *adapto-startup-projects*)
(push '("turtle"
        "turtle-hl"
        (("turtle_process_modules" "turtle-process-modules")))
      *adapto-startup-projects*)

;; Rosemacs repl shortcuts:
(global-set-key "\C-cl" '(lambda ()
                           (interactive)
                           (slime-ros)
                           (rosemacs-repl-mode t)))
(global-set-key "\C-cr"
                '(lambda ()
                 (interactive)
                 (slime-restart-inferior-lisp)))
(global-set-key "\C-cf"
                '(lambda ()
                 (interactive)
                 (slime-quit-lisp)))


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(slime-repl-output-face ((t (:inherit font-lock-string-face :foreground "white")))))

(setq frame-background-mode 'dark)

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-arjen)

;; AUCTEX 
;;For Auctex < 11.82 exchange ";;" in the following 2 lines
;;(require ’tex-site)
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t) ;; .pdf statt .dvi per default:
;;Zeilenumbruch
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;;Syntax Higlight
(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
;; Mathe Modus
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; Reftex einflechten und laden
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Satzende ". " statt ". ". " f¨ur M-k: l¨oschen bis Satzende usw.
(setq sentence-end "[.?!][]\"’)}]*\\($\\| \\| \\)[
;;]*") ;; Da ist ein "Newline in der Zeile!"
(setq sentence-end-double-space nil)
;;direkte Rechtschreib Korrektur:
;;(add-hook ’LaTeX-mode-hook ’flyspell-mode)
;; Nur benutzen falls Auctex > 11.81 mit preview-latex:
(load "preview-latex.el" nil t t)
;; aspell ist besser als ispell.
;; Zeile kommentieren, falls nicht installiert:
(setq-default ispell-program-name "aspell")
;; Deutsche Rechtschreibung falls \usepackage{ngerman}
;; oder german benutzt wird
(add-hook 'TeX-language-de-hook
(function (lambda () (ispell-change-dictionary "german8"))))

;;; DO THIS FIRST
;;; In .emacs, put the following line:
;;; (load-file "~/.emacs.d/vn_settings.el")
;;;
;;; Restart emacs
;;;
;;; Run M-x package-refresh-contents
;;;
;;; Run M-x package-install and then install: use-package,
;;; auto-complete, magit, smart-tabs-mode (general) elpy, py-autopep8
;;; (python), rust-mode (rust)
;;;
;;; If you change emacs settings through the GUI, copy the updated settings from ~/.emacs to ~/.emacs.d/vn_settings.el (this file).

;;; ---------- GENERIC SETTINGS
(delete-selection-mode t)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;;; ---------- PACKAGE ARCHIVES
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;			 ("melpa-stable" . "http://stable.melpa.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
;;                       ("marmalade" . "https://marmalade-repo.org/packages/"))
			 )
      )
;; (package-initialize)

;;; ---------- USE-PACKAGE FOR PACKAGE MANAGEMENT

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ; (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;;; ---------- MAGIT SPECIFIC
;; (require '

;;; ---------- TRAMP SPECIFIC
(setq tramp-default-method "ssh")

;;; ---------- RUST-MODE SPECIFIC
(require 'rust-mode)

;;; ---------- ORG-MODE SPECIFIC

;; org-babel language support
(org-babel-do-load-languages
'org-babel-load-languages
'((shell . t)))

;; org-mode markdown export
(require 'ox-md)
(global-set-key (kbd "C-c a") 'org-agenda)

;; org-mode easy code block templates
;; info from https://emacs.stackexchange.com/questions/12841/quickly-insert-source-blocks-in-org-mode
;; (require 'org-tempo)
;; replacement info: https://emacs.stackexchange.com/questions/46988/why-do-easy-templates-e-g-s-tab-in-org-9-2-not-work
;; in new org-mode, use C-c C-,

;;; ---------- LaTeX - SPECIFIC
(setq TeX-PDF-mode t)
;(turn-on-visual-line-mode)
(add-hook 'latex-mode-hook 'turn-on-visual-line-mode) ;; this doesn't work for some reason
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; this does work for now

;;; ---------- PYTHON/IPYTHON - SPECIFIC
(setq-default fill-column 80)
;;; DISABLE ELPY TEMPORARILY
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;;;; Black auto code formatter (can be handled by elpy)
;; (use-package python-black
;;   :demand t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim))

;;(when (require 'elpy nil t)
  ;; (elpy-enable)
  ;; (elpy-use-ipython)
  ;; (when (require 'py-autopep8 nil t)
  ;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  ;;   )
;;  )

;;; ---------- OB-IPYTHON - SPECIFIC
(setq org-confirm-babel-evaluate nil) ; no confirmation when code blocks are evaluated in org-mode
(when (require 'ob-ipython nil t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ipython . t)
     ;; other languages
     )
   )
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  )

;;; ---------- C/C++ MODE-SPECIFIC

;; Remove indentation for namespaces
(defconst my-cc-style
  '("k&r"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-mode" my-cc-style)
(setq c-default-style "my-cc-mode"
      c-basic-offset 4)

;; Smart-tabs mode
(smart-tabs-insinuate 'c 'c++)

;; Autocomplete
(require 'auto-complete)
(add-hook 'c-mode-hook (auto-complete-mode 1))
(add-hook 'c++-mode-hook (auto-complete-mode 1))
(global-auto-complete-mode t)

;; Flymake
;;(add-hook 'c-mode-hook flymake-mode)
;;(add-hook 'c++-mode-hook flymake-mode)

;;; ---------- KEYBOARD SHORTCUTS

; Flipping windows easily
(defun prev-window()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "C-'") 'prev-window)

; rebind org-mode and verilog-mode keys
(define-key (current-global-map) [remap org-cycle-agenda-files] 'prev-window)
(define-key (current-global-map) [remap electric-verilog-semi-with-comment] 'other-window)

;; comment/uncomment
(global-set-key (kbd "C-S-c") 'comment-or-uncomment-region)

;; recompile
(global-set-key (kbd "C-)") 'recompile)

;; Auto-fill mode
(global-set-key [f12] 'auto-fill-mode)

;; Unfill paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-S-q") `unfill-paragraph)

;; Icarus Verilog and gtkwave in verilog-mode

;; untabify the text
;; (add-hook 'verilog-mode-hook '(lambda ()
;;     (add-hook 'local-write-file-hooks (lambda()
;;        (untabify (point-min) (point-max))))))

(global-set-key [f6] 'iverilog-vvp-compile)
(global-set-key [f7] 'iverilog-vvp-compile-run-and-wave-view)
(global-set-key [f8] 'iverilog-vvp-compile-and-run)
(global-set-key [f9] 'iverilog-wave-view)

(defun file-title()
  "Return file title; eg returns asdf for /otp/asdf.txt ."
  (file-name-sans-extension(file-name-nondirectory (buffer-file-name))))

(defun iverilog-vvp-compile ()
  "Pass current verilog file (should be a testbench) to iverilog for compilation."
  (interactive)
  (if (or (string= (file-name-extension (buffer-file-name)) "v") (string= (file-name-extension (buffer-file-name)) "sv"))
      (shell-command(concat "iverilog -o icarus_compile/000_" (file-title) ".compiled \"" (buffer-file-name) "\" -Wall -g2005-sv"))
					;      (progn (shell-command(concat "iverilog \"" (buffer-file-name) "\" -o icarus_compile/000_" (file-title) ".compiled" ))
					;	     (shell-command (concat "vvp icarus_compile/000_" (file-title) ".compiled")) )
    (message "File isn't .v or .sv!") ) )

(defun iverilog-vvp-compile-and-run()
  "Compile and run current verilog file."
  (interactive)
  (if (or (string= (file-name-extension (buffer-file-name)) "v") (string= (file-name-extension (buffer-file-name)) "sv"))
      (progn (shell-command(concat "iverilog -o icarus_compile/000_" (file-title) ".compiled \"" (buffer-file-name) "\" -Wall  -g2005-sv"))
	     (shell-command (concat "vvp -N icarus_compile/000_" (file-title) ".compiled -lxt2")) ;add -lxt2 for LXT, -N for exiting with error code when $stop is called
	     )
    (message "File isn't .v or .sv!") ) )

(defun iverilog-vvp-compile-run-and-wave-view()
  "Compile and run current verilog file, then open GTKWAVE on the LXT2 file corresponding to current buffer, with matching save file (if available)."
  (interactive)
  (if (or (string= (file-name-extension (buffer-file-name)) "v") (string= (file-name-extension (buffer-file-name)) "sv"))
      (progn (shell-command(concat "iverilog -o icarus_compile/000_" (file-title) ".compiled \"" (buffer-file-name) "\" -Wall  -g2005-sv"))
	     (shell-command (concat "vvp -N icarus_compile/000_" (file-title) ".compiled -lxt2")) ;add -lxt2 for LXT, -N for exiting with error code when $stop is called
	     (shell-command (concat "gtkwave icarus_compile/000_" (file-title) ".lxt icarus_compile/001_" (file-title) ".sav &" )) )
    (message "File isn't .v or .sv!") ) )

(defun iverilog-wave-view()
  "Open GTKWAVE on the LXT2 file corresponding to current buffer, with matching save file (if available)."
  (interactive)
  (if (or (string= (file-name-extension (buffer-file-name)) "v") (string= (file-name-extension (buffer-file-name)) "sv"))
      (shell-command (concat "gtkwave icarus_compile/000_" (file-title) ".lxt icarus_compile/001_" (file-title) ".sav &" ) )
    (message "File isn't .v or .sv!") ) )

(defun iverilog-clean-files()
  "Clean files under the icarus_compile/ directory with extensions .compiled and .lxt"
  (interactive)
  (if (or (string= (file-name-extension (buffer-file-name)) "v") (string= (file-name-extension (buffer-file-name)) "sv"))
  (shell-command (concat "rm -v icarus_compile/*" (file-title) "*.compiled icarus_compile/*" (file-title) "*.lxt")) ; replace with .lxt for LXT
  (message "File isn't .v! Open the .v file whose subfiles you wish to clean!")))

;; Verilog-mode disable semicolon-and-enter behaviour

;;Verilog-mode customisations
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(require-final-newline t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(verilog-auto-lineup (quote declarations))
 '(verilog-auto-newline nil)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-p1800-keywords t))

;;;;; USEFUL KEYBOARD SHORTCUTS THAT I ALWAYS FORGET
;;
;; goto line: M-g g
;;
;; VC git blame: C-x v g
;; VC git checkout revision of file: C-x v ~
;; VC git diff file with checked-in revision C-x v =
;; VC git diff file with other revision of file: C-u x v =
;;
;; magit status buffer: C-x g
;; magit help, refresh, up, down: h, g, p, n
;; magit diff, stage, unstage: spc, s, u
;; magit commit, c
;; magit commit message: C-c C-c
;; magit fetch from upstream: F u
;; magit push to upstream: P u
;; magit log: l
;; magit branch: b
;; magit checkout a branch / local branch: b b / b l
;; magit checkout a local branch based on remote (like checkout -b) : b c
;; magit merge: m
;;
;; diff reverse direction: R
;; diff next/previous hunk: n / p
;; diff apply hunk: C-c C-a
;;
;; org-mode src block: C-c C-, s
;; org-mode agenda: C-c a
;;
;; elpy-black-fix-code: M-x elpy-black-fix-code
;;
;; emacs set macro counter to number: C-x C-k C-c <num>
;; emacs add number to macro counter: C-x C-k C-a <num>
;; emacs insert macro counter and don't increment it: C-u 0 C-x C-k TAB
;; emacs swap characters: C-t
;;
;;;;; USEFUL LINUX TIPS AND TRICKS
;;
;; Search for a file in pacman: pacman -F <file>
;;
;; Use Mac keyboard in Arch Linux more conveniently: setxkbmap -option altwin:left_meta_win [does not work for all keybindings]

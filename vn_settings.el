;;; vn_settings.el --- Vlad's Emacs settings -*- lexical-binding: t; -*-

;;; DO THIS FIRST
;;; In .emacs (or init.el), put the following line:
;;;   (load "~/.emacs.d/vn_settings")
;;;
;;; Restart emacs.  use-package is built in to Emacs 29+ — for older
;;; Emacs the snippet below will install it.  Other packages to install
;;; with M-x package-install: magit, smart-tabs-mode, elpy, py-autopep8
;;; (python), rust-mode (rust).
;;;
;;; If you change emacs settings through the GUI, copy the updated
;;; settings from ~/.emacs to ~/.emacs.d/vn_settings.el (this file).

;;; ---------- PACKAGE ARCHIVES
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; Emacs 27+ auto-runs package-initialize before init; no explicit call needed.

(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

;; Most packages should load lazily; override with :demand t when needed.
(setq use-package-always-defer t)

;;; ---------- GENERIC SETTINGS
(delete-selection-mode 1)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq-default fill-column 80)

;;; ---------- TRAMP
(setq tramp-default-method "ssh")

;;; ---------- COMPLETION (company)
(use-package company
  :hook (after-init . global-company-mode))

;;; ---------- RUST
(use-package rust-mode
  :mode "\\.rs\\'")

;;; ---------- ORG
(use-package org
  :bind ("C-c a" . org-agenda)
  :config
  (require 'ox-md)
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))
;; org-mode easy code block templates: use C-c C-, (since org 9.2)

;;; ---------- LaTeX / Text
(setq TeX-PDF-mode t)
;; LaTeX-mode (AUCTeX) and latex-mode both derive from text-mode, so this covers both.
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

;;; ---------- C/C++
(defconst my-cc-style
  '("k&r"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-mode" my-cc-style)
(setq c-default-style "my-cc-mode"
      c-basic-offset 4)
;; (smart-tabs-insinuate 'c 'c++)

;;; ---------- KEYBOARD SHORTCUTS

;; Window flipping
(defun prev-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-;") #'other-window)
(global-set-key (kbd "C-'") #'prev-window)
(define-key (current-global-map) [remap org-cycle-agenda-files] #'prev-window)
(define-key (current-global-map) [remap electric-verilog-semi-with-comment] #'other-window)

;; Comment / uncomment
(global-set-key (kbd "C-S-c") #'comment-or-uncomment-region)

;; Recompile
(global-set-key (kbd "C-)") #'recompile)

;; Auto-fill mode
(global-set-key [f12] #'auto-fill-mode)

;; Unfill paragraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-S-q") #'unfill-paragraph)

;;; ---------- VERILOG / IVERILOG

(defun file-title ()
  "Return file title; eg returns asdf for /otp/asdf.txt ."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(defun vn--verilog-buffer-p ()
  "Non-nil if the current buffer visits a .v or .sv file."
  (let ((ext (file-name-extension (or (buffer-file-name) ""))))
    (or (string= ext "v") (string= ext "sv"))))

(defun iverilog-vvp-compile ()
  "Pass current verilog file (should be a testbench) to iverilog for compilation."
  (interactive)
  (if (vn--verilog-buffer-p)
      (shell-command
       (concat "iverilog -o icarus_compile/000_" (file-title)
               ".compiled \"" (buffer-file-name) "\" -Wall -g2005-sv"))
    (message "File isn't .v or .sv!")))

(defun iverilog-vvp-compile-and-run ()
  "Compile and run current verilog file."
  (interactive)
  (if (vn--verilog-buffer-p)
      (progn
        (shell-command
         (concat "iverilog -o icarus_compile/000_" (file-title)
                 ".compiled \"" (buffer-file-name) "\" -Wall -g2005-sv"))
        ;; -lxt2 for LXT, -N to exit with error code when $stop is called.
        (shell-command
         (concat "vvp -N icarus_compile/000_" (file-title) ".compiled -lxt2")))
    (message "File isn't .v or .sv!")))

(defun iverilog-vvp-compile-run-and-wave-view ()
  "Compile and run current verilog file, then open GTKWAVE on the LXT2 file."
  (interactive)
  (if (vn--verilog-buffer-p)
      (progn
        (shell-command
         (concat "iverilog -o icarus_compile/000_" (file-title)
                 ".compiled \"" (buffer-file-name) "\" -Wall -g2005-sv"))
        (shell-command
         (concat "vvp -N icarus_compile/000_" (file-title) ".compiled -lxt2"))
        (shell-command
         (concat "gtkwave icarus_compile/000_" (file-title)
                 ".lxt icarus_compile/001_" (file-title) ".sav &")))
    (message "File isn't .v or .sv!")))

(defun iverilog-wave-view ()
  "Open GTKWAVE on the LXT2 file corresponding to current buffer."
  (interactive)
  (if (vn--verilog-buffer-p)
      (shell-command
       (concat "gtkwave icarus_compile/000_" (file-title)
               ".lxt icarus_compile/001_" (file-title) ".sav &"))
    (message "File isn't .v or .sv!")))

(defun iverilog-clean-files ()
  "Clean files under icarus_compile/ with extensions .compiled and .lxt."
  (interactive)
  (if (vn--verilog-buffer-p)
      (shell-command
       (concat "rm -v icarus_compile/*" (file-title) "*.compiled "
               "icarus_compile/*" (file-title) "*.lxt"))
    (message "File isn't .v! Open the .v file whose subfiles you wish to clean!")))

(global-set-key [f6] #'iverilog-vvp-compile)
(global-set-key [f7] #'iverilog-vvp-compile-run-and-wave-view)
(global-set-key [f8] #'iverilog-vvp-compile-and-run)
(global-set-key [f9] #'iverilog-wave-view)

;;; ---------- CUSTOM
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

(provide 'vn_settings)

;;;;; USEFUL KEYBOARD SHORTCUTS THAT I ALWAYS FORGET
;;
;; newline character in search: C-q C-j
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
;; emacs shell comint clear buffer: C-c M-o
;;
;;;;; USEFUL LINUX TIPS AND TRICKS
;;
;; Search for a file in pacman: pacman -F <file>
;;
;; Use Mac keyboard in Arch Linux more conveniently: setxkbmap -option altwin:left_meta_win [does not work for all keybindings]

;;; vn_settings.el ends here

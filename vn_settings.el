;;; DO THIS FIRST
;;; In .emacs, put the following line:
;;; (load-file "~/.emacs.d/vn_settings.el")
;;;
;;; Run M-x package-install auto-complete

;;; ---------- GENERIC SETTINGS
(delete-selection-mode t)

;;; ---------- MARMALADE PACKAGE ARCHIVE
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; ---------- LaTeX - SPECIFIC
(setq TeX-PDF-mode t)

;;; ---------- C MODE-SPECIFIC
(setq c-default-style "linux"
      c-basic-offset 4)

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

;; Auto-fill mode
(global-set-key [f12] 'auto-fill-mode)

;; Icarus Verilog and gtkwave
(global-set-key [f6] 'iverilog-vvp-compile)
(global-set-key [f7] 'gtkwave-view-current)
(global-set-key [f8] 'iverilog-run-vvp)

(defun file-title()
  "Return file title; eg returns asdf for /otp/asdf.txt ."
  (file-name-sans-extension(file-name-nondirectory (buffer-file-name))))

(defun iverilog-vvp-compile ()
  "Pass current verilog file (should be a testbench) to iverilog for compilation."
  (interactive)
  (if (string-equal (file-name-extension (buffer-file-name)) "v")
      (shell-command(concat "iverilog \"" (buffer-file-name) "\" -o icarus_compile/000_" (file-title) ".compiled" ))
					;      (progn (shell-command(concat "iverilog \"" (buffer-file-name) "\" -o icarus_compile/000_" (file-title) ".compiled" ))
					;	     (shell-command (concat "vvp icarus_compile/000_" (file-title) ".compiled")) )
    (message "File isn't .v!") ) )

(defun iverilog-run-vvp()
  "Open GTKWAVE on the VCD/LXT2 file corresponding to current buffer, with matching save file (if available)."
  (interactive)
  (if (string-equal (file-name-extension (buffer-file-name)) "v")
      (progn (shell-command(concat "iverilog \"" (buffer-file-name) "\" -o icarus_compile/000_" (file-title) ".compiled" ))
	     (shell-command (concat "vvp icarus_compile/000_" (file-title) ".compiled -lxt2")) ;add -lxt2 for LXT
	      )
    (message "File isn't .v!") ) )

(defun gtkwave-view-current()
  "Open GTKWAVE on the LXT file corresponding to current buffer, with matching save file (if available)."
  (interactive)
  (if (string-equal (file-name-extension (buffer-file-name)) "v")
      (progn (shell-command(concat "iverilog \"" (buffer-file-name) "\" -o icarus_compile/000_" (file-title) ".compiled" ))
	     (shell-command (concat "vvp icarus_compile/000_" (file-title) ".compiled -lxt2")) ;add -lxt2 for LXT
	     (shell-command (concat "gtkwave icarus_compile/000_" (file-title) ".lxt icarus_compile/001_" (file-title) ".sav &" )) )
    (message "File isn't .v!") ) )

(defun iverilog-clean-files()
  (interactive)
  "Clean files under the icarus_compile/ directory with extensions .compiled and .lxt"
  (if (string-equal (file-name-extension (buffer-file-name)) "v")
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
 '(verilog-auto-lineup (quote declarations))
 '(verilog-auto-newline nil)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-p1800-keywords t))


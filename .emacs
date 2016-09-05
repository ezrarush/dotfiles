(require 'cask "/usr/local/Cellar/cask/0.7.4/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


(setq user-full-name "Ezra Rush")
(setq user-mail-address "rushwest@gmail.com")

;; (setq default-directory "C:/home/" )

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "~/.emacs.d/lisp/scratch-message")

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(require 'package)
;; (add-to-list 'package-archives
;;   '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; replace with use-package package because reasons
(setq my-package-list '(solarized-theme
			coffee-mode
			jump-char
			org-page
		        icicles
			flymake-ruby))
;; (mapc #'package-install my-package-list)

;; eshell does not have same env as os term bash
(setenv "PATH"
  (concat
   "/usr/local/bin" ":"
   (getenv "PATH") ; inherited from OS
  )
)

;; Dired tries to guess a default target directory. This means: if there is a Dired buffer displayed in the next window, use its current directory, instead of this Dired buffer's current directory.
(setq dired-dwim-target t)

;; c-x c-f when point looks like a host address will ping and time out
;; https://github.com/syl20bnr/spacemacs/issues/2654
(setq ffap-machine-p-known 'reject)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; (require 'helm-config)
;; (helm-mode 1)

(global-set-key (kbd "C-c r r") 'inf-ruby)
(add-hook 'inferior-ruby-mode-hook 'ansi-color-for-comint-on)

(require 'org-page)
(setq op/repository-directory "~/code/org-page")
(setq op/site-domain "http://blog.ezrarush.com/")
;;; for commenting, you can choose either disqus or duoshuo
(setq op/personal-disqus-shortname "")
;; (setq op/personal-duoshuo-shortname "your_duoshuo_shortname")
;;; the configuration below are optional
(setq op/personal-google-analytics-id "")

;; ruby syntax checking
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

;; cf needs html highlighting
(add-to-list 'auto-mode-alist '("\\.cfm" . html-mode))

;; yaml
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; coffeescript mode - use spaces only, with 2 spaces per indentation level. Never mix tabs and spaces
;; The default CoffeeScript mode makes terrible choices. This turns everything into 2 space indentations and makes it so the mode functions rather than causing you indentation errors every time you modify a file. 
(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

;; chicken scheme slime
;; (add-to-list 'load-path "/var/lib/chicken/6/")   ; Where Eggs are installed
;; (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)

;; We also want to enable the SLIME minor mode in Scheme files:

;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;            (slime-mode t)))


;;
;; ace jump mode major function
;; 
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)



;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; firefox like tabbing
(global-set-key (kbd "<C-tab>") 'icicle-other-window-or-frame)

(setq truncate-partial-width-windows nil)

;;========================== echo-keys =========================
(defvar *echo-keys-last* nil "Last command processed by `echo-keys'.")

(defun echo-keys ()
  (interactive)
  (let ((deactivate-mark deactivate-mark))
    (when (this-command-keys)
      (with-current-buffer (get-buffer-create "*echo-key*")
	(goto-char (point-max))
	;; self  self
	;; self  other \n
	;; other self  \n
	;; other other \n
	(unless (and (eq 'self-insert-command *echo-keys-last*)
		     (eq 'self-insert-command this-command))
	  (insert "\n"))
	(if (eql this-command 'self-insert-command)
	    (let ((desc (key-description (this-command-keys))))
	      (if (= 1 (length desc))
		  (insert desc)
		(insert " " desc " ")))
	  (insert (key-description (this-command-keys)))
	  (insert " " (prin1-to-string (key-binding (this-command-keys))))
	  )
	(setf *echo-keys-last* this-command)
	(dolist (window (window-list))
	  (when (eq (window-buffer window) (current-buffer))
	    ;; We need to use both to get the effect.
	    (set-window-point window (point))
	    (end-of-buffer)))))))


(defun toggle-echo-keys ()
  (interactive)
  (if (member 'echo-keys  pre-command-hook)
      (progn
	(remove-hook 'pre-command-hook 'echo-keys)
	(dolist (window (window-list))
	  (when (eq (window-buffer window) (get-buffer "*echo-key*"))
	    (delete-window window))))
    (progn
      (add-hook    'pre-command-hook 'echo-keys)
      (delete-other-windows)
      (split-window nil (- (window-width) 32) t)
      (other-window 1)
      (switch-to-buffer (get-buffer-create "*echo-key*"))
      
      (set-window-dedicated-p (selected-window) t)
      (other-window 1))))

;; (global-set-key (kbd "<f8>") 'slime-switch-to-output-buffer)

;; <f1> is for help, home-row C-h is better used as delete
(keyboard-translate ?\C-h ?\C-?)

(column-number-mode t)
(show-paren-mode t)
(blink-cursor-mode 0)
(setq x-stretch-cursor t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


;;turn down the time to echo keystrokes, less waiting?
(setq echo-keystrokes 0.1)

(setq visible-bell t)

(winner-mode 1)

;;

(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key (kbd "<f9>") 'recentf-open-files)

;; recentf lists files that were opened but never displayed
;; this attempts to fix 
(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
      (let ((display-count (buffer-local-value 'buffer-display-count buf)))
        (if (> display-count 0) display-count nil)))))

(defsubst keep-default-and-visible-recentf-p (file)
  "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
  (if (recentf-keep-default-predicate file)
      (file-was-visible-p file)))

;; When a buffer is closed, remove the associated file from the recentf
;; list if (1) recentf would have, by default, removed the file, or
;; (2) the buffer was never displayed.  This is useful because, for
;; example, CEDET opens a lot of files in the background to generate
;; its tags database, etc.
(setq recentf-keep '(keep-default-and-visible-recentf-p))

;; end of recentf

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;; 
(add-hook 'lisp-mode-hook
          (lambda () (local-set-key (kbd "C-w") #'backward-kill-sexp)))

(global-hl-line-mode 1)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;;diable the welcome message
(setq inhibit-startup-message t)

;;format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %f (%b)")

;; (set-face-attribute 'default nil :font "Inconsolata-12")
;; use y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;;====== jump-char =======

(require 'jump-char)
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

;;====== color-theme ======

(load-theme 'solarized-dark t)
;; Or alternate color palette
;; (load-theme 'solarized-light t)

(defun solarized-dark ()
  (interactive)
  (load-theme 'solarized-dark t))

(defun solarized-light ()
  (interactive)
  (load-theme 'solarized-light t))

;;========= icicles ======================

;(add-to-list 'load-path "~/icicles")
;(require 'icicles)
(icy-mode 1)

;;=========== ParEdit ===========

;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; found @ emailataskcom youtube video

;; (defun backward-up-list+ ()
;;   "Stupid backward-up-list doesn't work from inside a string and i got tired of having to move outside the string to use it."
;;   (interactive)
;;   (if (in-string-p)
;;       (while (in-string-p)
;; 	(backward-char))
;;     (backward-up-list)))


;;=========== Electric RETURN =============

; I DUNNO IF ALREADY BUILT INTO PAREDIT (prob. is already built-in)

;; (defvar electrify-return-match
;;     "[\]}\)\"]"
;;     "If this regexp matches the text after the cursor, do an \"electric\"
;;   return.")
 
;; (defun electrify-return-if-match (arg)
;;     "If the text after the cursor matches `electrify-return-match' then
;;   open and indent an empty line between the cursor and the text.  Move the
;;   cursor to the new line."
;;     (interactive "P")
;;     (let ((case-fold-search nil))
;;       (if (looking-at electrify-return-match)
;; 	  (save-excursion (newline-and-indent)))
;;       (newline arg)
;;       (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
  ;; (global-set-key (kbd "RET") 'electrify-return-if-match)

;;========= Lisp env and SLIME ============

;; (setq inferior-lisp-program "sbcl")
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;; ;; Tell Emacs how to auto-load SLIME when needed 
;; (when (load "slime-autoloads" t)
;;   (setq slime-auto-connect 'always)
;;   (slime-setup '(slime-fancy slime-asdf inferior-slime)))

;; ;;     Making slime connect to your lisp automatically when you open a lisp file.

;; ;; (defun cliki:start-slime ()
;; ;;   (unless (slime-connected-p)
;; ;;     (save-excursion (slime))))

;; ;; (add-hook 'slime-mode-hook 'cliki:start-slime)


;; ;; Stop SLIME's REPL from grabbing DEL,
;; ;; which is annoying when backspacing over a '('
;; (defun override-slime-repl-bindings-with-paredit ()
;;   (define-key slime-repl-mode-map
;;     (read-kbd-macro paredit-backward-delete-key) nil))
;; (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;Fontify *SLIME Description* buffer for SBCL
;; (defun slime-description-fontify ()
;;       "Fontify sections of SLIME Description."
;;       (with-current-buffer "*SLIME Description*"
;;         (highlight-regexp
;;          (concat "^Function:\\|"
;;                  "^Macro-function:\\|"
;;                  "^Its associated name.+?) is\\|"
;;                  "^The .+'s arguments are:\\|"
;;                  "^Function documentation:$\\|"
;;                  "^Its.+\\(is\\|are\\):\\|"
;;                  "^On.+it was compiled from:$")
;;          'hi-green-b)))
;;     (defadvice slime-show-description (after slime-description-fontify activate)
;;       "Fontify sections of SLIME Description."
;;       (slime-description-fontify))

;;Improve usability of slime-apropos: slime-apropos-minor-mode

;; (defvar slime-apropos-anchor-regexp "^[^ ]")
;;     (defun slime-apropos-next-anchor ()
;;       (interactive)
;;       (let ((pt (point)))
;;         (forward-line 1)
;;         (if (re-search-forward slime-apropos-anchor-regexp nil t)
;;             (goto-char (match-beginning 0))
;;           (goto-char pt)
;;           (error "anchor not found"))))
;;     (defun slime-apropos-prev-anchor ()
;;       (interactive)
;;       (let ((p (point)))
;;         (if (re-search-backward slime-apropos-anchor-regexp nil t)
;;             (goto-char (match-beginning 0))
;;           (goto-char p)
;;           (error "anchor not found"))))
;;     (defvar slime-apropos-minor-mode-map (make-sparse-keymap))
;;     (define-key slime-apropos-minor-mode-map "\C-m" 'slime-describe-symbol)
;;     (define-key slime-apropos-minor-mode-map "l" 'slime-describe-symbol)
;;     (define-key slime-apropos-minor-mode-map "j" 'slime-apropos-next-anchor)
;;     (define-key slime-apropos-minor-mode-map "k" 'slime-apropos-prev-anchor)
;;     (define-minor-mode slime-apropos-minor-mode "")
;;     (defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
;;       ""
;;       (when (get-buffer "*SLIME Apropos*")
;;         (with-current-buffer "*SLIME Apropos*" (slime-apropos-minor-mode 1))))

;;Remove SLIME's keybinding on C-c x

;; (add-hook 'slime-mode-hook 
;;             (defun slime-sanitize-bindings ()
;;               "Removes SLIME's keybinding on C-c x"
;;               (cond ((boundp 'slime-mode-map)
;;                      (define-key slime-mode-map (kbd "C-c x") nil)
;;                      (message "slime keybinding on C-c x has been sanitized"))
;;                     ('t (message "slime keybindings not sanitized")))))


;;Integrate yas/expend to TAB key

;; (defun slime-tab ()
;;       "slime-mode tab dwim, either indent, complete symbol or yas/expand"
;;       (interactive)
;;       (let ((r (slime-indent-and-complete-symbol)))
;; ;;         (unless r
;; ;;           (yas/expand))))
;; ;;     (defun my-slime-mode-hook ()
;; ;;       (interactive)
;; ;;       (define-key slime-mode-map (kbd "<tab>")
;; ;;         'slime-tab)
;; ;;       )
;; ;;     (add-hook 'slime-mode-hook 'my-slime-mode-hook)

;; ;(setq common-lisp-hyperspec-root  "file:/usr/local/doc/HyperSpec/" )

(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(fci-rule-color "#424242")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(icicle-command-abbrev-alist (quote ((ace-jump-word-mode eh 1))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

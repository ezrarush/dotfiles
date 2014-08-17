(setq default-directory "C:/home/quicklisp/local-projects/" )

(add-to-list 'load-path "~/.emacs.d")

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; chicken scheme slime
(add-to-list 'load-path "/var/lib/chicken/6/")   ; Where Eggs are installed
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)

;; We also want to enable the SLIME minor mode in Scheme files:

(add-hook 'scheme-mode-hook
          (lambda ()
           (slime-mode t)))


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



;; can i make firefox like emacs or emacs like firefox :(
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


;;=============== *scratch* and be changed back/better at anytime ==============

(setq initial-scratch-message (nth (random 82) 
				   '(";; When the going gets weird, the weird turn professional.\n"
				     ";; The Edge... there is no honest way to explain it because the only people who really know where it is are the ones who have gone over.\n"
				     ";; I wouldn't recommend sex, drugs or insanity for everyone, but they've always worked for me.\n"
				     ";; Freedom is something that dies unless it's used.\n"
				     ";; It never got weird enough for me.\n"
				     ";; No sympathy for the devil; keep that in mind. Buy the ticket, take the ride…and if it occasionally gets a little heavier than what you had in mind, well…maybe chalk it off to forced conscious expansion: Tune in, freak out, get beaten.\n"
				     ";; Life has become immeasurably better since I have been forced to stop taking it seriously.\n"
				     ";; Who is the happier man, he who has braved the storm of life and lived or he who has stayed securely on shore and merely existed?"
				     ";; Never turn your back on fear. It should always be in front of you, like something that might need to be killed.\n"
				     ";; If you're going to be crazy, you have to get paid for it or else you're going to be locked up.\n"
				     ; 10
				     ";; It used to be thought that you could judge someone’s character by looking at the shape of his head. Whether or not this is true of people, it is generally true of Lisp programs.\n"
				     ";; If you want side-effects, use setq on the return value.\n"
				     ";; Treat the following operators as if there were a tax on their use: \n;;   set setq setf psetf psetq incf decf push pop pushnew\n;;   rplaca rplacd rotatef shiftf remf remprop remhash\n;; and also let*, in which imperative programs often lie concealed. Treating these operators as taxable is only proposed as a help toward, not a criterion for, good Lisp style.\n"
				     ";; \"let\" is a sign that bad things are to follow. Like eval at runtime, uninitialized variables are so rarely needed that they should generally be treated as a symptom of some illness in the program. Such variables are often used like pins which hold the program down and keep it from coiling into its natural shape.\n"
				     ";; Define the general and pass the specific as an argument.\n"
				     ";; If brevity is the soul of wit, it is also, along with efficiency, the essence of good software.\n"
				     ";; The muses, like cooks, spring into action at the sight of ingredients.\n"
				     ";; In programming, the best way to learn is often to begin experimenting as soon as possible. A full theoretical understanding can come later.\n"
				     ";; For programs in general, to have good style is to be clear and efficient.\n"
				     ";; Gotos are condemned in Lisp precisely because it’s so easy to hide them: you can use do instead, and if you didn’t have do, you could write it.\n"
				     ; 20
				     ";; Patterns are just the sort of thing computers are good at. Why should you bother following them when you could have a program do it for you?\n"
				     ";; The villain carries his hostage down to the dungeon, and the rescuing hero carries her back up again; they both follow the same path, but in different directions. It’s not surprising if people have the impression that setf must work this way, because all the predefined inversions seem to be of this form; indeed, place is the conventional name for a parameter which is to be inverted. In principle, setf is more general: an access form and its inversion need not even operate on the same data structure.\n"
				     ";; When we can predict what code would be evaluated at runtime, we can simply generate it at compile-time.\n"
				     ";; The advantage of programming in a high-level language: expressive power well-matched to the problem at hand.\n"
				     ";; a skeptic’s notion of truth: some facts are known, and everything else is false.\n"
				     ";; Perfection is attained not when there is nothing left to add but when there is nothing left to take away.\n"
				     ";; The initial version of a Lisp program can be like a diamond: small, clear, and very expensive. There may be a great temptation to leave it that way.\n"
				     ";; Lisp is like living in a rich country instead of a poor one: it may seem unfortunate that one has to work to stay thin, but surely this is better than working to stay alive, and being thin as a matter of course.\n"
				     ";; The best programming language abstractions save not just typing, but thought.\n"
				     ";; Any inaccuracies in this may be explained by the fact that it has been prepared with the help of a computer.\n"
				     ; 30
				     ";; Is it possible that software is not like anything else, that it is meant to be discarded: that the whole point is to always see it as a soap bubble?\n"
				     ";; A computer is like a violin. You can imagine a novice trying first a phonograph and then a violin. The latter, he says, sounds terrible. That is the argument we have heard from our humanists and most of our computer scientists. Computer programs are good, they say, for particular purposes, but they aren't flexible. Neither is a violin, or a typewriter, until you learn how to use it.\n"
				     ";; The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 3. The third is separating them from all other ideas that accompany them in their real existence: this is called abstraction, and thus all its general ideas are made.\n"
				     ";; There is a popular, widespread belief that computers can do only what they are programmed to do. This false belief is based on a confusion between form and content. A rigid grammar need not make for precision in describing processes. The programmer must be very precise in following the computer grammar, but the content he wants to be expressed remains free. The grammar is rigid because of the programmer who uses it, not because of the computer. The programmer does not even have to be exact in his own ideas‑he may have a range of acceptable computer answers in mind and may be content if the computer's answers do not step out of this range. The programmer does not have to fixate the computer with particular processes. In a range of uncertainty he may ask the computer to generate new procedures, or he may recommend rules of selection and give the computer advice about which choices to make. Thus, computers do not have to be programmed with extremely clear and precise formulations of what is to be executed, or how to do it.\n"
				     ";; We now come to the decisive step of mathematical abstraction: we forget about what the symbols stand for. ...[The mathematician] need not be idle; there are many operations which he may carry out with these symbols, without ever having to look at the things they stand for.\n"
				     ";; Even while it changes, it stands still.\n"
				     ";; .. It's in words that the magic is -- Abracadabra, Open Sesame, and the rest -- but the magic words in one story aren't magical in the next. The real magic is to understand which words work, and when, and for what; the trick is to learn the trick. \n;; ... And those words are made from the letters of our alphabet: a couple-dozen squiggles we can draw with the pen. This is the key! And the treasure, too, if we can only get our hands on it! It's as if -- as if the key to the treasure is the treasure!\n"
				     ";; the only way to learn how to program is to program\n"
				     ";; theory can come later\n"
				     ";; you cannot serve two masters\n"
				     ; 40
				     ";; mèdeis ageômetrètos eisitô mou tèn stegèn\n;; let no one inapt to geometry come in\n"
				     ";; for Plato, geometry, as well as all other mathematical sciences, is not an end in itself, but only a prerequisite meant to test and develop the power of abstraction in the student, that is, his ability to go beyond the level of sensible experience which keeps us within the \"visible\" realm, that of the material world, all the way to the pure intelligible. And geometry can also make us discover the existence of truths that may be said to be \"transcendant\" in that they don't depend upon what we may think about them, but have to be accepted by any reasonable being, which should lead us into wondering whether such transcendant truths might not exist as well in other areas, such as ethics and matters relating to men's ultimate happiness, whether we may be able to \"demonstrate\" them or not.\n"
				     ";; The best way to predict the future is to invent it.\n"
				     ";; Don’t Panic!\n"
				     ";; The purpose of computing is insight, not numbers.\n"
				     ";; festina lente\n"
				     ";; All problems in computer science can be solved by another level of indirection, except for the problem of too many layers of indirection. – David J. Wheeler\n"
				     ";; Programming is understanding. – Kristen Nygaard\n"
				     ";; The road goes ever on and on. – Bilbo Baggins\n"
				     ";; Language shapes the way we think, and determines what we can think about. – B.L.Whorf\n"
				     ; 50
				     ";; Why waste time learning when ignorance is instantaneous? – Hobbes"
				     ";; ‘‘... I have long entertained a suspicion, with regard to the decisions of philosophers\n;; upon all subjects, and found in myself a greater inclination to dispute, than assent to\n ;; their conclusions. There is one mistake, to which they seem liable, almost without\n;; exception; they confine too much their principles, and make no account of that vast\n;; variety, which nature has so much affected in all her operations. When a philosopher\n;; has once laid hold of a favourite principle, which perhaps accounts for many natural\n;; effects, he extends the same principle over the whole creation, and reduces to it every\n ;; phænomenon, though by the most violent and absurd reasoning. ...’’ – David Hume, Essays, Moral, Political, and Literary. PART I. (1752)\n"
				     ";; Perfection is achieved only on the point of collapse. – C. N. Parkinson\n"
				     ";; The sublime and the ridiculous are often so nearly related that it is difficult to class them separately. – Thomas Paine\n"
				     ";; Form a more perfect Union. – The people\n"
				     ";; Death to all fanatics!\n"
				     ";; Form must follow function. – Le Corbusier\n"
				     ";; ... there is nothing more difficult to carry out, nor more doubtful of success, nor more\n;; dangerous to handle, than to initiate a new order of things. For the reformer makes\n;; enemies of all those who profit by the old order, and only luke warm defenders in all\n;; those who would profit by the new order...’’ — Niccol o ` Machiavelli\n"
				     ";; Ignorance more frequently begets confidence than does knowledge. - Charles Darwin\n"
				     ";; When I use a word it means just what I choose it to mean – neither more nor less. – Humpty Dumpty\n"
				     ; 60
				     ";; We are all special cases. – Albert Camus\n"
				     ";; Abstraction is selective ignorance. – Andrew Koenig\n"
				     ";; Premature optimization is the root of all evil.\n"
				     ";; For every complex problem, there is an answer that is clear, simple, and wrong. – H. L. Mencken\n"
				     ";; Trips to fairly unknown regions should be made twice; once to make mistakes and once to correct them. – John Steinbeck\n"
				     ";; Never express yourself more clearly than you are able to think. – Niels Bohr\n"
				     ";; Many secrets of art and nature are thought by the unlearned to be magical. – Roger Bacon\n"
				     ";; Form is liberating. – Engineer ́s proverb\n"
				     ";; The reason that STL containers and algorithms work so well together is that they know nothing of each other. – Alex Stepanov\n"
				     ";; Anyone can have an idea; it ́s what you do with the idea that ́s the important thing. – Terry Pratchett\n"
				     ; 70
				     ";; The time you enjoy wasting is not wasted time. – Bertrand Russell\n"
				     ";; Prefer the standard to the offbeat. – Strunk & White\n"
				     ";; Keep it simple: as simple as possible, but no simpler. – A. Einstein\n"
				     ";; Keep Calm and Carry On. – English slogan\n"
				     ";; Share early; share often.\n"
				     ";; The engines don’t move the ship at all. The ship stays where it is and the engines move the universe around it. Futurama\n"
				     ";; two young newlyweds were preparing to enjoy their first baked ham dinner in their new apartment. After unwrapping the meat and setting it on the cutting board, the wife chopped off both ends of the ham with a butcher knife, tossing the two small ends in the garbage can. \"Wait a minute,\" said the mystified husband. \"Why did you do that? Why did you just cut off the ends of the ham like that?\" \"I don't know. My mother always did,\" answered the wife. \"Maybe it helps bring out the flavor.\" Unsatisfied with this answer, the husband called his mother-in-law. \"Can you tell me why you cut the two ends off of a ham before you cook it?\" \"Well,\" said the mother, \"I'm not really sure why. That's just the way my mother did her ham, and it was always delicious.\" As soon as he hung up he called his wife's grandmother. \"Grandma, we have an important question for you. Can you tell us why you cut the ends off of a ham before you cook it?\" \"Oh my yes, dear,\" answered Grandma in her quiet, thin voice. \"I cut the ends of the ham off so it would fit in my pan.\"\n"
				     ";; Hacking your program is no substitute for understanding your problem. - Richard A. O'Keefe\n"
				     ";; Good style replaces the need for great memory - Peter Norvig\n"
				     ";; Package complexity into objects and abstractions; not global variables/dependencies\n"
				     ; 80
				     ";; If you have to look up the default value, you need to supply it. You should only take the default if you truly believe you don't care of if you're sure the default is well-understood and well-accepted by all.\n"
				     ";; Maximize LOCNW\n")))

(global-set-key (kbd "<f8>") 'slime-switch-to-output-buffer)

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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
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

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; found @ emailataskcom youtube video

(defun backward-up-list+ ()
  "Stupid backward-up-list doesn't work from inside a string and i got tired of having to move outside the string to use it."
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
	(backward-char))
    (backward-up-list)))


;;=========== Electric RETURN =============

; I DUNNO IF ALREADY BUILT INTO PAREDIT (prob. is already built-in)

(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")
 
(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
	  (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))

;; Using local-set-key in a mode-hook is a better idea.
  (global-set-key (kbd "RET") 'electrify-return-if-match)



;;========= Lisp env and SLIME ============

(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;     Making slime connect to your lisp automatically when you open a lisp file.

;; (defun cliki:start-slime ()
;;   (unless (slime-connected-p)
;;     (save-excursion (slime))))

;; (add-hook 'slime-mode-hook 'cliki:start-slime)


;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

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
;;         (unless r
;;           (yas/expand))))
;;     (defun my-slime-mode-hook ()
;;       (interactive)
;;       (define-key slime-mode-map (kbd "<tab>")
;;         'slime-tab)
;;       )
;;     (add-hook 'slime-mode-hook 'my-slime-mode-hook)

;(setq common-lisp-hyperspec-root  "file:/usr/local/doc/HyperSpec/" )


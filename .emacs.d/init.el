(defconst jmn-config-location "~/dotfiles/emacs.org"
  "Location of literate org file used to automaitically tangle to init.el")

(defconst jmn-gtd-directory "~/Documents/gtd/"
  "Location of gtd org files: projects, inbox, next,  whip, journal, and habits")

(defconst jmn-connected-systems '("lat" "dsk" "xps")
  "Systems which should download packages. Others get the 'pure' configuration.")

(defconst jmn-dark-mode t
  "Do we want Emacs in a dark mode? Note: no dark-mode for windows as of now")

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name jmn-config-location))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(if (member system-name jmn-connected-systems)
    (defconst jmn-pure nil "Indicating if we are pure or using packages")
  (defconst jmn-pure t "Indicating if we are pure or using packages"))

;; flag
(defconst jmn-term (not (display-graphic-p (selected-frame)))
  "Indicating if emacs is being run within a terminal or not")

(if jmn-pure
    (defmacro use-package (&rest _))  ;; define use-package macro to do nothing
  (progn
    (require 'package)
    (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
    (package-initialize)

    (unless package-archive-contents
      (package-refresh-contents))

    (unless (package-installed-p 'use-package)
      (package-install 'use-package));; Initialize use-package on non-Linux platforms

    (require 'use-package)
    (setq use-package-always-ensure t) ; no need for :ensure t for each package.
    (setq use-package-verbose t)) ; log configure/loading messages in *Messages*
  )

;; allows us to make sure environment packages are installed
(use-package use-package-ensure-system-package
  :ensure t)

;; update your packages with=M-x auto-package-update-now= to update right now.

;;;; Basic ;;;;
(setq inhibit-startup-message t)		; inhibit startup message
(tool-bar-mode -1)                            ; remove toolbar
(menu-bar-mode -1)				; Disable the menu bar
(scroll-bar-mode -1)                          ; remove side scrollbar
(tooltip-mode -1)				; Disable tooltips
(set-fringe-mode 6)				; Give some breathing room
(setq visible-bell t)                         ; Set up the visible bell
(save-place-mode 1)				; Open file where last visited
(setq Buffer-menu-name-width 35)		; give name more room
(setq-default indicate-empty-lines t)		; indicate long lines
(defalias 'yes-or-no-p 'y-or-n-p)               ; Make  =yes or no= prompts shorter
(column-number-mode 1)                          ; show column number in modeline
(winner-mode 1)                                 ; redo and undo window changes
(if (version<= "28" emacs-version)
    (repeat-mode 1))                 ; multi-key sequences can be repeated

;; hooks
(if jmn-pure
    (add-hook 'before-save-hook 'whitespace-cleanup)); non-pure uses ws-butler
(unless (eq system-type 'windows-nt)
  (add-hook 'text-mode-hook 'flyspell-mode))	; enable spellcheck on text mode

;; The following help syncing
(global-auto-revert-mode 1)			; refresh buffer if changed on disk
(setq auto-revert-use-notify nil)		; don't notify?
(setq auto-revert-verbose nil)			;

;; Set scratch buffer to be in org-mode and modify initial message
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "**Scratch Buffer**\n\n")

;; Open text files in Org-Mode
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq backup-directory-alist
      '( ("." . "~/.emacs.d/filebackups")))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (if (version<= "29" emacs-version)
      (set-frame-parameter nil 'alpha-background value)
    (set-frame-parameter (selected-frame) 'alpha value)))

;;;; Scrolling ;;;;
;; Fully redraw the display before it processes queued input events.
(setq redisplay-dont-pause            t)

;; Number of lines of continuity to retain when scrolling by full screens
(setq next-screen-context-lines       2)  ;; golden ration pkg will replaced this if loaded

;; only 'jump' when moving this far off the screen
(setq scroll-conservatively         100)
(setq scroll-step                     1) ;; Keyboard scroll one line at a time
(setq mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse        t) ;; Scroll window under mouse
(setq fast-but-imprecise-scrolling    t) ;; No (setq less) lag while scrolling lots.
(setq auto-window-vscroll           nil) ;; Cursor move faster
(setq pixel-scroll-precision-mode     1) ;; pixel based scrolling

(use-package fast-scroll
  :defer 2
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package undo-tree
  :defer 2
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil)) ;; don't save ~undo-tree~ file

;;;; Modeline ;;;;
(use-package all-the-icons
  :defer 1
  :init
  (when (and (not (member "all-the-icons" (font-family-list))) ;; autoinstall fonts
             (window-system))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)
           (doom-modeline-vcs-max-length 20))) ;; default is 12

;;;; Cleanup whitespace only on lines I touched ;;;;
(use-package ws-butler
    :defer 4
    :hook ((text-mode . ws-butler-mode)
           (prog-mode . ws-butler-mode)))

;; general improvements
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'buffer-menu)   ;; open buffer menue in current buffer
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)   ;; "C-x k" asks which buffer
(global-set-key (kbd "C-o") 'other-window)  ;; default is "C-x o"
(global-set-key (kbd "M-o") 'previous-multiframe-window)
(global-set-key (kbd "C-c C-c") 'eval-buffer)
(global-set-key (kbd "C-c C-r") 'eval-region)

;; make font bigger/smaller.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; f1 is a leader key for help
;; f2 is a leader key for 2 columns
;; f3 is kmacro-start-macro-or-insert-counter -- start recording a macro
;; f4 ends the macro recording
;; f5-f10 are clear by default
;; f11 is toggle full screen
;; f12 is clear by default

;; See recentfiles
(global-set-key (kbd "<f5>") 'recentf-open-files)

;; writing/editing
(global-set-key (kbd "<f9>") 'ispell-word)
(global-set-key (kbd "<f10>") 'dictionary-lookup-definition)

;; Buffer-menu-mode
(define-key Buffer-menu-mode-map (kbd "C-o") 'other-window)
(define-key Buffer-menu-mode-map (kbd "M-o") 'previous-multiframe-window)
;; "o" opens in another buffer and moves focus
;; "C-M-o" opens in another buffer and keeps focus in the Buffer-menu
(define-key Buffer-menu-mode-map (kbd "C-M-o") 'Buffer-menu-switch-other-window)

;; xref
(with-eval-after-load 'xref
  (define-key xref--xref-buffer-mode-map (kbd "C-o") 'other-window)
  (define-key xref--xref-buffer-mode-map (kbd "o") 'xref-show-location-at-point))

;; bookmark-bmenue
(with-eval-after-load 'bookmark
  (define-key bookmark-bmenu-mode-map (kbd "C-o") 'other-window)
  (define-key bookmark-bmenu-mode-map (kbd "M-o") 'previous-multiframe-window)
  (define-key bookmark-bmenu-mode-map (kbd "C-M-o") 'bookmark-bmenu-switch-other-window))

;; compilation-modes
(defun jmn-compilation-keybindings()
  (define-key compilation-mode-map (kbd "C-o") 'other-window)
  (define-key compilation-mode-map (kbd "C-M-o") 'compilation-display-error))

(add-hook 'compilation-mode-hook 'jmn-compilation-keybindings)

;; grep-mode
(defun jmn-grep-keybindings()
  (define-key grep-mode-map (kbd "o") 'compilation-display-error)
  (define-key grep-mode-map (kbd "C-o") 'other-window))

(add-hook 'grep-mode-hook #'jmn-grep-keybindings)

;; adjust windows with keybindings
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)

;; non-org C-c <blank> bindings
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c =") 'vc-diff) ;; also bound to "C-x v ="

;;;; Dired ;;;;
(add-hook 'dired-mode-mode 'dired-hide-details-mode) ;; hide default -- '(' to toggle
(with-eval-after-load 'dired
  (setq dired-listing-switches "-agho --group-directories-first" )
  (setq find-ls-option '("-print0 | xargs -0 ls -agho" . ""))
  (setq dired-dwim-target t) ;; guess other dired directory for copy and rename
  (setq wdired-allow-to-change-permissions t)
  (define-key dired-mode-map (kbd "C-o") 'other-window)
  (setq dired-guess-shell-alist-user '(
                                       ("\\.png\\'" "shotwell")
                                       ("\\.jpg\\'" "shotwell")
                                       ("\\.jpeg\\'" "shotwell")
                                       ("\\.mp4\\'" "vlc")
                                       ("\\.avi\\'" "vlc")
                                       ("\\.iso\\'" "vlc")
                                       ("\\.webm\\'" "vlc")
                                       ("\\.mkv\\'" "vlc")
                                       ("\\.mp3\\'" "rhythmbox")
                                       ("\\.html\\'" "firefox")
                                       ("\\.epub\\'" "ebook-viewer")
                                       ("\\.pdf\\'" "evince")
                                       ("\\.ipynb\\'" "code"))))

(use-package all-the-icons-dired
  :after dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;; sluggish mode which lists the recursive size of each folder/item in dired.
(use-package dired-du
  :commands dired-du-mode
  :defer t
  :config (setq dired-du-size-format t))

;; use a single dired session
(use-package dired-single
  :after dired
  :defer t
  :hook (dired-mode .
                    (lambda () (define-key dired-mode-map [remap dired-find-file]
                                           'dired-single-buffer)
                      (define-key dired-mode-map
                                  [remap dired-mouse-find-file-other-window]
                                  'dired-single-buffer-mouse)
                      (define-key dired-mode-map [remap dired-up-directory]
                                  'dired-single-up-directory))))

;;;; Proced ;;;;
(defun proced-settings ()
    (proced-toggle-auto-update 5)) ;; auto update every 4 seconds

(add-hook 'proced-mode-hook 'proced-settings)

;;;; Native comp ;;;;
(setq native-comp-async-report-warnings-errors nil)

(use-package goto-last-change
  :ensure t
  :bind ("C-c g" . goto-last-change))

(use-package ivy
  :after counsel
  :defer t
  :config
  (ivy-mode 1)
  ;; remove ^ on the inputbuffer
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel
  :defer t
  :bind (("M-x" . counsel-M-x)      ; displays ivy-rich info in minibuffer
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         ))

(use-package helpful
  :defer 3
  :commands (helpful-callable helpful-variavle helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :defer 1
  :config(which-key-mode)
  (setq which-key-idle-delay 0.8))

(defun jmn-set-font-height(value)
  (interactive "nFont height (default is 100): ")
  (set-face-attribute  'default nil :height value))

(jmn-set-font-height (alist-get (system-name) '(
                                                ("xps" . 110)
                                                ("dsk" . 110)
                                                ("lat" . 115))
                                100 nil 'string=)) ;; default is 100

(setq text-scale-mode-step 1.05)

(defun jmn-load-init ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun jmn/vscode-current-buffer-file-at-point ()
  (interactive)
  (start-process-shell-command "code"
                               nil
                               (concat "code --goto "
                                       (buffer-file-name)
                                       ":"
                                       (number-to-string (line-number-at-pos))
                                       ":"
                                       ;; +1 who knows why
                                       (number-to-string (+ 1 (current-column))))))

(define-key global-map (kbd "<f12>")
            'jmn-vscode-current-buffer-file-at-point)

(defun jmn-remove-M-1-9-from-mode-map (modemap)
  "remove 'M-[d]' keybinding from a mode-map"
  (dolist (num '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (define-key modemap (kbd (concat "M-" num)) nil)))

(use-package dtrt-indent
  :hook (prog-mode . (lambda () (dtrt-indent-mode 1))))

;;Prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode) ;; hide show, use C-c @ prefix
(unless (eq system-type 'windows-nt)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)) ;; flyspell-comments

(show-paren-mode    1) ; Highlight parentheses pairs.

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
    (org-mode . (lambda () (rainbow-delimiters-mode 0))))
  )

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package magit
  :commands (magit-status))
  ;; :custom
  ;; (magit-display-buffer-function
  ;;    #'magit-display-buffer-same-window-except-diff-v1));;display status buffer


;; moved here incase magit installed on a pure machine ;; replace original keybinding?
(with-eval-after-load 'magit
  (add-hook 'magit-status-mode-hook
            (lambda () (define-key magit-mode-map (kbd "C-<tab>") nil)))
  (jmn-remove-M-1-9-from-mode-map magit-mode-map))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
         (org-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (set-face-background 'git-gutter-fr:deleted  (face-background 'default))
  (set-face-foreground 'git-gutter-fr:deleted  "red"))

(use-package highlight-indent-guides
  :defer 2
  :hook prog-mode
  :config
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-auto-odd-face-perc -7)
  (setq highlight-indent-guides-auto-even-face-perc 7))

(use-package company
  :ensure t
  :hook (eglot-managed-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.5))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode +1))

(use-package treemacs
  :ensure t
  :defer t
  :bind (("C-c t" . treemacs ))
  :config
  (setq treemacs-width 30))

(use-package eglot
  :defer 1
  :ensure-system-package
  ((bash-language-server . "sudo dnf install -y nodejs-bash-language-server")
   (pyright . "pip install pyright")
   (clangd . "sudo dnf install -y clang-tools-extra"))
  :hook
  ((sh-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)))

(use-package cmake-mode
  :defer t
  :ensure-system-package ((cmake . "sudo dnf install -y cmake"))
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config (cmake-font-lock-activate))

(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1))

  (use-package yasnippet-snippets
    :after yas-minor-mode) ; load basic snippets from melpa

;; free up C-; for goto-last change
;; (with-eval-after-load 'flyspell
;;     (define-key flyspell-mode-map (kbd "C-;") nil))

(use-package evil-nerd-commenter
:bind ("M-;". evilnc-comment-or-uncomment-lines))

(use-package tree-sitter-langs
  :hook ((sh-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (c-mode . tree-sitter-hl-mode)
         (c++-mode . tree-sitter-hl-mode)))

(use-package ripgrep
  :defer 2
  :ensure-system-package
  ((rg . "sudo dnf install -y ripgrep")))

(use-package yaml-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package devdocs
 :defer 2
 :bind  ("C-h D" . 'devdocs-lookup))

(defun jmn-tab-complete-mode ()
  "My tab complete config"
  (setq tab-always-indent 'complete) ;;complete if indented
  (add-to-list 'completion-styles 'initials t))

;; turning it on everywhere and only turning company mode on for prog-mode
;;;; if this stayes it will just be moved up to under the 'General Emacs'
(jmn-tab-complete-mode)

(if jmn-pure
    (global-set-key (kbd "C-c g") (lambda () (interactive)
                                    (vc-dir (file-name-directory (buffer-file-name))))))

(defun jmn-vc-commit ()
    "My command to show the vc-diff along with the commit input"
    (interactive)
    (vc-next-action (buffer-file-name))
    (previous-multiframe-window)
    (vc-diff)
    (previous-multiframe-window)
    (switch-to-buffer"*vc-log*")
    ;; (set-window-text-height (selected-window) 4)
    ;; (kill-buffer "*log-edit-files*")
    )

(with-eval-after-load 'vc-dir
  (define-key vc-dir-mode-map (kbd "C-o") 'other-window)
  (define-key vc-dir-mode-map (kbd "C-M-o") 'vc-dir-display-file)
  (define-key vc-dir-mode-map (kbd "c") 'jmn-vc-commit))

(defun my-sh-mode-hook-fn()
  (setq sh-basic-offset 2
        sh-indentation 2)) ;; defaults is 4
(add-hook 'sh-mode-hook #'my-sh-mode-hook-fn)

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2
        sh-indentation 2)) ;; defaults are 4

(use-package pyvenv
:ensure t
:defer t
:diminish
:config

(setenv "WORKON_HOME" "/home/ape/.conda/envs")
        ; Show python venv name in modeline
        (setq pyvenv-mode-line-indicator
              '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
        (pyvenv-mode t))

;;;; Python-mode ;;;;
(with-eval-after-load 'python
  (setq python-shell-completion-native-enable 1)
  (define-key python-mode-map (kbd "C-RET") 'python-shell-send-statement)
  (if (executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (setq python-shell-interpreter-args "-i --simple-prompt"))))

(defun my-python-mode-hook-fn ()
  (with-eval-after-load 'devdocs
    (setq-local devdocs-current-docs '("python~3.12"))))


(add-hook 'python-mode-hook #'my-python-mode-hook-fn)

(setq compilation-scroll-output t) ;;*Compilation* buffer scroll with the output.

(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                    buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq-default c-basic-offset 2)

(defun my-c++-mode-hook-fn ()
  (with-eval-after-load 'devdocs
    (setq-local devdocs-current-docs '("cpp"))))

(defun my-c-mode-hook-fn ()
  (with-eval-after-load 'devdocs
    (setq-local devdocs-current-docs '("c"))))

(add-hook #'c-mode-hook #'my-c-mode-hook-fn)
(add-hook #'c++-mode-hook #'my-c++-mode-hook-fn)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(use-package markdown-mode
   :defer t
   :hook (markdown-mode-hook . (lambda ()
                                 (flyspell-mode))))

(use-package  markdown-preview-mode
  :defer t)

(defun jmn/org-mode-setup ()
  (unless jmn-term
    (org-indent-mode))
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  ;; fix issue where it matches > with partentheses -- may break blocks with <>
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> ".")
  )

(defun jmn/org-font-setup ()
  (unless jmn-term ;; Replace list hyphen with dot
    (font-lock-add-keywords
     'org-mode '(("^ *\\([-]\\) "
                  (0 (prog1 () (compose-region (match-beginning 1)
                                               (match-end 1) "•")))))))

  ;; Set faces for heading levels
  (let ((jmn-org-hfont-alist '((gnu/linux . "Cantarell")
                              (windows-nt . unspecified))))
    (dolist (face '((org-level-1 . 1.2)
                    (org-level-2 . 1.1)
                    (org-level-3 . 1.1)
                    (org-level-4 . 1.1)
                    (org-level-5 . 1.1)
                    (org-level-6 . 1.1)
                    (org-level-7 . 1.1)
                    (org-level-8 . 1.1)
                    ))
      (set-face-attribute (car face) nil
                          :font (alist-get system-type jmn-org-hfont-alist)
                          :weight 'regular :height (cdr face))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  ;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch) ;;warning
  (set-face-attribute 'org-block nil :foreground 'unspecified' :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(add-hook 'org-mode-hook  'jmn/org-mode-setup)

(with-eval-after-load 'org
  (jmn/org-font-setup)
  (unless (or jmn-term jmn-pure)
    (setq org-ellipsis " ▾"))
  (setq org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2 ;; I undo this somewhere 4tangling
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil
        org-list-indent-offset 1
        org-image-actual-width nil    ;; fix to allow picture resizing
        org-return-follows-link t
        org-use-speed-commands t
        org-use-property-inheritance t  ;; preperties to affect nested sections
        org-export-babel-evaluate nil ;; don't run src blocks on export
        org-agenda-tags-column (alist-get
                                (system-name) '(("xps" . -85)
                                                ("dsk" . -90))
                                'auto nil 'string=)))

(unless jmn-term
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))))

(defun jmn/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . jmn/org-mode-visual-fill))

(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                                     '((shell  . t)
                                       (python . t)
                                       (makefile . t)
                                       (latex  . t)
                                       (C      . t))))

(setq org-confirm-babel-evaluate nil)

(with-eval-after-load 'org
  (require 'org-tempo);; This is needed as of Org 9.2
  (add-to-list 'org-structure-template-alist '("la" . "src latex"))
  (add-to-list 'org-structure-template-alist '("m" . "src makefile"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python  :results output"))
  (add-to-list 'org-structure-template-alist '("pyim" . "src python :results file :var f=strNameWithDoubleQuotes
import matplotlib.pyplot as plt
plt.savefig(f)
f"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++  :includes <iostream>"))
  (add-to-list 'org-structure-template-alist '("cppnm" . "src C++  :main no")))

(defconst jmn-latex-scale 1.2 "scaling factor for latex fragments")
(setq org-format-latex-options (plist-put org-format-latex-options :scale jmn-latex-scale))

(defun update-org-latex-fragments ()
  (org-latex-preview '(64))
  (plist-put org-format-latex-options :scale
             (+ jmn-latex-scale  (* 0.3 text-scale-mode-amount)))
  (org-latex-preview '(16)))
(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(global-set-key (kbd "C-c x") #'org-html-export-to-html)
(global-set-key (kbd "C-c s") #'org-store-link)
(global-set-key (kbd "C-c i") #'org-insert-link)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package htmlize
  :ensure t
  :defer t)

;; initialize if not already
(if (not (bound-and-true-p jmn-org-files-to-html-on-save))
    (setq  jmn-org-files-to-html-on-save nil))

(defun jmn-org-export-html-on-save-list()
  (when (member (buffer-file-name)  jmn-org-files-to-html-on-save)
    (org-html-export-to-html)))

(add-hook 'after-save-hook #'jmn-org-export-html-on-save-list)

(defun jmn-export-to-html-on-save()
  (interactive)
  (add-to-list 'jmn-org-files-to-html-on-save (buffer-file-name)))

(add-to-list 'safe-local-variable-values '(eval jmn-export-to-html-on-save))

(if (and (version<= "29" emacs-version) (not jmn-pure))
    (with-eval-after-load 'org
      (add-to-list 'org-safe-remote-resources  "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'")))

;; Org Agenda
(setq org-agenda-window-setup 'other-window) ;; other good option: reorganize-frame
;; Exited with ‘q’ or ‘x’ and the old state is restored.
(setq org-agenda-restore-windows-after-quit 1)
(setq org-agenda-span 'day)

;; SOMEDAY itmes are ommitted from GTD interface on purpose
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|"
                                    "DONE(d)" "IGNORE(i)")))

(require 'find-lisp) ;; may not be needed
(setq org-agenda-files (find-lisp-find-files jmn-gtd-directory "\.org$"))

;; level/maxlevel = order in hierarchy
(setq org-refile-targets
      '(("projects.org" :maxlevel . 2)
        ("someday.org" :maxlevel . 1)
        ("whip.org" :level . 0)
        ("next.org" :level . 0)))

;; https://github.com/syl20bnr/spacemacs/issues/3094
(setq org-refile-use-outline-path 'file org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-prefix-format '((agenda . " %i %-10:c%t [%e]% s ")
                                 (todo . " %i %-10:c [%-4e] ")
                                 (tags . " %i %-12:c")))

(setq org-deadline-warning-days 30)

(setq org-agenda-start-with-log-mode t)  ;; allows us to see closed in calendar
(setq org-log-done 'time)  ;; creates CLOSED time tag
(setq org-log-into-drawer t)  ;; creates a LOGBOOK drawer for notes
(setq org-agenda-use-time-grid nil)  ;; no grid at top of agenda

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda ""
                  ((org-deadline-warning-days 30)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Un-Scheduled Tasks")))
          (todo "TODO"
                ((org-agenda-overriding-header "Active Un-Scheduled Tasks")))))

        (" " "Agenda"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-deadline-warning-days 365)))

          (todo "TODO"
                ((org-agenda-overriding-header "To Refile")
                 (org-agenda-files (list (concat jmn-gtd-directory "inbox.org")))))

          (todo "NEXT"
                ((org-agenda-overriding-header "In Progress")
                 (org-agenda-files (list (concat jmn-gtd-directory "projects.org")
                                         (concat jmn-gtd-directory "next.org")
                                         (concat jmn-gtd-directory "inbox.org")))))

          (todo "TODO"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-files (list (concat jmn-gtd-directory  "projects.org")))))

          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files (list (concat jmn-gtd-directory  "next.org"))))
                (org-agenda-skip-function '(org-agenda-skip-entry-if
                                            'deadline 'scheduled)))

          (todo "HOLD"
                    ((org-agenda-overriding-header "HOLD")
                     (org-agenda-files (list (concat jmn-gtd-directory "projects.org")
                                             (concat jmn-gtd-directory "next.org")
                                             (concat jmn-gtd-directory "inbox.org")))))
          ))))

(defun jmn-someday() "Quick access to someday.org (no links in agenda)"
       (interactive)
       (find-file (concat jmn-gtd-directory "someday.org")))

(setq org-agenda-todo-ignore-scheduled 'all) ;; cant get it to work for deadlines

;; org habit;;
(add-to-list 'org-modules 'org-habit)
;;(require 'org-habit)
(with-eval-after-load 'org-habit
  (setq org-habit-graph-column
        (alist-get (system-name) '(("xps" . 56)
                                   ("dsk" . 52))
                   50 nil 'string=))) ;; default is 40

(setq org-capture-templates
      `(("t" "Todo [inbox]" entry
         (file ,(concat jmn-gtd-directory "inbox.org"))
         "* TODO %i%?" :empty-lines 1)

        ("T" "Todo Today [inbox]" entry
           (file ,(concat jmn-gtd-directory "inbox.org"))
           "* TODO %?\nDEADLINE: %t" :empty-lines 1)

        ("l" "Linked Todo [inbox]" entry
         (file ,(concat jmn-gtd-directory "inbox.org"))
         "* TODO %i%? \n %a" :empty-lines 1)

        ("s" "Schedule" entry
         (file+headline ,(concat jmn-gtd-directory "whip.org")  "Whip")
         "* %i%? \n %U %^t" :empty-lines 1)

        ("j" "Journal" entry
         (file+datetree ,(concat jmn-gtd-directory "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a"  :empty-lines 1)))

(defun org-archive-done-tasks-tree ()
  "Archive all DONE tasks in the current org tree"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(defun org-archive-done-tasks-file ()
  "Archive all DONE tasks in the current org file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

;; could set in the inbox header instead (where tags are set)
(customize-set-variable 'org-global-properties
                        '(("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 4:00")))

(defun jmn/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (interactive)
  (org-with-wide-buffer  ; what does this do?
   ;; (org-agenda-set-tags) ; may want in the future
   (org-agenda-priority)
   (org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(global-set-key (kbd "C-c p") 'jmn/org-agenda-process-inbox-item)

(defmacro func-ignore (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))

(advice-add 'org-archive-done-tasks-tree
            :after (func-ignore #'org-save-all-org-buffers))
(advice-add 'org-archive-done-tasks-file
            :after (func-ignore #'org-save-all-org-buffers))
(advice-add 'org-refile
            :after (func-ignore #'org-save-all-org-buffers))
(advice-add 'org-deadline
            :after (func-ignore #'org-save-all-org-buffers))
(advice-add 'org-schedule
            :after (func-ignore #'org-save-all-org-buffers))
(advice-add 'org-store-log-note
            :after (func-ignore #'org-save-all-org-buffers))
(advice-add 'org-todo
            :after (func-ignore #'org-save-all-org-buffers))

;; if agenda is already open, update it with new capture;; work?
(advice-add 'org-capture-finalize
             :after (func-ignore #'org-agenda-redo-all))
;; ;; (advice-add 'org-capture-finalize
;; ;;             :after (func-ignore #'org-agenda-redo-all))

(defun jmn-agenda (&optional arg) (interactive "P") (org-agenda arg " "))

(global-set-key (kbd "C-c a") 'jmn-agenda)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-keymap (kbd "o") 'org-agenda-goto)) ;; more like dired

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-o") 'other-window)
  (define-key term-raw-map (kbd "M-o") 'previous-multiframe-window)

  ;; access prev commandsin line mode, though line and char-mode rings are separate
  (define-key term-raw-map (kbd "M-p") 'term-send-up)
  (define-key term-raw-map (kbd "M-n") 'term-send-down)

  ;; add "C-x" as escape character and use it for keybindings
  (let (term-escape-char)
    (term-set-escape-char ?\C-x))
  (define-key term-raw-map (kbd "C-x C-k") ' kill-current-buffer))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :ensure-system-package ((cmake . "sudo dnf install -y cmake")
                          (libtool . "sudo dnf install -y libtool" )) ;; compilation
  :defer t
  :bind (:map vterm-mode-map ("C-o" . other-window))
  :config
  (jmn-remove-M-1-9-from-mode-map vterm-mode-map)
  (setq vterm-max-scrollback 10000)
  :init (let ((package "cmake"))
           (unless (executable-find package)
             (async-shell-command (concat "sudo dnf install -y " package))))
         (let ((package "libtool"))
           (unless (executable-find package)
             (async-shell-command (concat "sudo dnf install -y " package)))))


(use-package vterm-toggle
  :after vterm
  :defer t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (global-set-key (kbd "C-`") 'vterm-toggle)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer
                                        bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(setq custom-safe-themes t) ;; don't ask if theme is safe

(defun jmn-disable-theme()
  (interactive)
  (disable-theme (car custom-enabled-themes)))

(use-package gruvbox-theme)

(defun jmn-set-gruv-org-faces (props)
  "Function used by all gruvbox themes for setting or faces"

  (with-eval-after-load 'org
    (set-face-foreground 'org-priority (face-foreground font-lock-constant-face))
    (set-face-foreground 'org-agenda-done (alist-get 'done-color props))
    (set-face-foreground 'org-headline-done (alist-get 'done-color props))
    (set-face-foreground 'org-done (alist-get 'done-color props))

    (set-face-attribute 'org-block-begin-line nil :inherit 'font-lock-comment-face)
    (set-face-attribute 'org-block-end-line nil :inherit 'font-lock-comment-face)
    (set-face-background 'org-block-begin-line (face-background `default))
    (set-face-background 'org-block-end-line (face-background `default))

    (setq org-todo-keyword-faces
          `(("NEXT" . ,(face-foreground font-lock-function-name-face))
            ("HOLD" . ,(face-foreground font-lock-builtin-face))
            ("DONE" . ,(alist-get 'done-color props))))

    (if (alist-get 'org-block props)
        (set-face-background 'org-block (alist-get 'org-block props)))))


(defun jmn-load-gruvbox-dark-medium ()
  "Theme for dark time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-dark-medium t)
  (set-face-background 'line-number
                       (face-attribute 'default :background))

  ;; (set-face-foreground 'default "gray75") ;; default "#ebdbb2"
  (set-face-foreground 'default "moccasin") ;; default "#ebdbb2"
  (set-face-foreground 'font-lock-comment-face  "#98be65") ;; default "#ebdbb2"
  (jmn-set-gruv-org-faces '((done-color . "gray35" ))))


(defun jmn-load-gruvbox-dark-hard ()
  "Theme for very dark time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-dark-hard t)

  (set-face-foreground 'default "bisque2") ;; default "#ebdbb2"

  (set-face-background 'mode-line-inactive "gray22")
  (set-face-background 'mode-line-active   "gray35")

  (set-face-background 'line-number
                       (face-attribute 'default :background))
  (set-face-background 'fringe
                       (face-attribute 'default :background))

  (set-face-foreground 'font-lock-comment-face  "#98be65") ;; default "#ebdbb2"
  (set-face-foreground 'font-lock-string-face  "LightGoldenrod3")
  (set-face-foreground 'font-lock-builtin-face  "Orange3")
  (jmn-set-gruv-org-faces '((done-color . "gray35" )
                            (org-block-begin-line . "gray14")
                            (org-block-end-line . "gray14")
                            (org-block . "gray7"))))

(defun jmn-load-gruvbox-light-medium()
  "Theme for light time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-light-medium t)

  (jmn-set-gruv-org-faces '((done-color . "Navajowhite3" ))))

(defun jmn-load-gruvbox-light-hard()
  "Theme for very light time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-light-hard t)

  (jmn-set-gruv-org-faces '((done-color . "Navajowhite3" )
                            (org-block . "#fbf1c7")))) ;; default "#f9f5d7"

  (defun jmn-load-gruvbox-light-soft()
  "Theme for very light time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-light-soft t)

  (jmn-set-gruv-org-faces '((done-color . "Navajowhite3" )
                            (org-block . "#ebdbb2")))) ;; default "#f9f5d7"

(defun jmn-load-pure-dark-theme()
  (load-theme 'wombat)
  (with-eval-after-load 'org
    (set-face-background 'org-block "gray8"))
  (with-eval-after-load 'tab-bar
    (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive))
      (set-face-attribute  face nil
                           :foreground (face-foreground 'default)
                           :background (face-background 'default)
                           :weight 'regular))
    (set-face-background 'tab-bar-tab "gray25")))

(defun jmn-load-pure-light-theme()
  (with-eval-after-load 'org
    (set-face-background 'org-block "gray93")))

(if (window-system)
    (set-frame-height (selected-frame) 54))

(if jmn-pure
    (if jmn-dark-mode
        (if (eq system-type 'gnu/linux)  ;; pure dark linux
            (jmn-load-pure-dark-theme)
          (progn ;; pure dark windows
            (print "No dark-mode for windows- setting light mode")
            (jmn-load-pure-light-theme)))
      (jmn-load-pure-light-theme))  ;; pure light

  (if jmn-dark-mode  ;; non-pure and dark
      (jmn-load-gruvbox-dark-hard)
    (jmn-load-gruvbox-light-medium)))  ;; non-pure and light

(if (eq system-type 'gnu/linux)
    (setq shell-command-switch "-ic"))  ;; add -i so it loads .bashrc (aliases!)

(cond
 ((eq system-type 'windows-nt)
  ;;set font
  (when (member "Consolas" (font-family-list))
    (set-frame-font "Consolas" t t))

  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) ;; needed?
  (add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)  ;; needed?
  ;; (setq explicit-shell-file-name "c:/Program Files/Git/bin/bash.exe") ;; gitbash

  (setq explicit-shell-file-name "c:/Program Files/Emacs/libexec/emacs/27.2/x86_64-w64-mingw32/cmdproxy.exe") ;; Emacs recommended
  (setq shell-file-name explicit-shell-file-name)

  ;; give emacs access to gnu coreutils, though should already be in =path=
  (add-to-list 'exec-path "c:/Program Files/Git/") ;; git-bash, git-cmd
  (add-to-list 'exec-path "c:/Program Files/Git/bin") ;; bash
  (add-to-list 'exec-path "c:/Program Files/Git/usr/bin") ;; coreutils (diff, ls, etc.)
  (add-to-list 'exec-path "c:/Program Files/Python312") ;; python-- not working

  ;; likey no octave, so read only matlab files
  (add-hook 'octave-mode-hook 'read-only-mode)

  ;; external ls for directories first support (dired) -- may not be needed with above
  (setq ls-lisp-use-insert-directory-program t)  ;; use external ls
  (setq inserft-directory-program "c:/Program Files/Git/user/bin/ls.exe") ;; ls loc
  ))

(defun jmn-set-background-unspecified ()
  "Set background of buffer and line numbers to unspecified"
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-background 'line-number "unspecified-bg"))

(if jmn-term
    (add-hook 'window-setup-hook 'jmn-set-background-unspecified))

(recentf-mode 1) ;; needed for recent files in dashboard
(setq recentf-max-menu-items 25) ;; max number of entries
(run-at-time nil (* 5 60) 'recentf-save-list) ;; save recent files periodically

;; Exclude the following files from the recents list
(add-to-list 'recentf-exclude "~/.emacs.d/recentf")
(add-to-list 'recentf-exclude (concat jmn-gtd-directory "inbox.org"))
(add-to-list 'recentf-exclude (concat jmn-gtd-directory "next.org"))
(add-to-list 'recentf-exclude (concat jmn-gtd-directory "whip.org"))
(add-to-list 'recentf-exclude (concat jmn-gtd-directory "someday.org"))
(add-to-list 'recentf-exclude (concat jmn-gtd-directory "journal.org"))
(add-to-list 'recentf-exclude (concat jmn-gtd-directory "habits.org"))
(add-to-list 'recentf-exclude
             (concat (file-name-directory jmn-config-location) "emacs.html"))

(if jmn-pure
    (progn
      ;; display recent files on startup
      ;; (add-hook 'after-init-hook (lambda () (recentf-open-files)))
      ;; display projects.org on startup
      (add-hook 'after-init-hook
                (lambda () (find-file
                            (concat jmn-gtd-directory "projects.org"))))
      ;; make display recent files the dashboard command
      (global-set-key (kbd "C-c d") 'recentf-open-files)))

(if (version<= "27" emacs-version)
    (progn
      (setq tab-bar-close-button-show nil        ; remove close button
            tab-bar-show 1                       ; only show tab bar if #tabs>1
            tab-bar-select-tab-modifiers '(meta) ; Alt-i switch to the tab numbered i
            tab-bar-tab-hints t)                 ; show a number on each tabs

      (tab-bar-mode 1)
      (tab-bar-close-other-tabs))) ; ensures (tab-bar-mode 1) works on older systems


(defun jmn-create-my-tabs()
  "Create my default tabs"
  (interactive)
  (tab-close-other)
  (delete-other-windows)
  (tab-bar-rename-tab "org")
  (find-file (concat jmn-gtd-directory "projects.org"))
  (jmn-agenda)

  (tab-bar-new-tab)
  (tab-bar-rename-tab "workspace")
  (switch-to-buffer "projects.org")

  (tab-bar-new-tab)
  (tab-bar-rename-tab "term")
  (if jmn-pure
      (term "/bin/bash")
    (vterm))

  (tab-bar-new-tab)
  (tab-bar-rename-tab "config")
  (find-file jmn-config-location)
  (magit-status)

  (tab-bar-select-tab-by-name "org"))

(use-package dashboard
  :ensure t
  :init     (dashboard-setup-startup-hook)
  :bind ( "C-c d" . dashboard-open)
  :config
  (setq dashboard-startup-banner 2)  ;; (nil . no-banner)  ([1-5] . plain-text banners)
  (setq dashboard-center-content 1)
  (setq dashboard-show-shortcuts 1)  ;; show the single-character shortcuts
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))

  (setcdr (assoc 'projects dashboard-item-shortcuts) "j")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'project-el)
  (dashboard-modify-heading-icons '((recents . "file-text")))
  (setq dashboard-set-footer nil))

(defun my-dashboard-hook()
  "Needed to define these after hook for some reason"
  (define-key dashboard-mode-map (kbd "n")  'dashboard-next-line)
  (define-key dashboard-mode-map (kbd "p")  'dashboard-previous-line))

(add-hook 'dashboard-mode-hook 'my-dashboard-hook)

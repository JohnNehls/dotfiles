(defconst jmn-config-location "~/dotfiles/emacs.org"
  "Location of literate org file used to automaitically tangle to init.el")

(let ((gtd-path '((gnu/linux . "~/Documents/gtd/")
                 (windows-nt . "c:/Users/nehlsj/OneDrive/Documents/gtd/"))))
  (defconst jmn-gtd-directory (alist-get system-type gtd-path)
  "Location of gtd org files: projects, inbox, next,  whip, journal, and habits"))

(defconst jmn-connected-systems '("lat" "dsk" "xps")
  "Systems which should download packages. Others get the 'pure' configuration.")

(defconst jmn-connected-extras t
  "Flag of weather to use the purely astetic packages or not on connected system")

(defconst jmn-pureplus-systems '("lat")
  "Systems which use the pure setup with the plus packages")

(defconst jmn-dark-mode t
  "Do we want Emacs in a dark mode? Note: no dark-mode for windows as of now")

(defconst jmn-font-height-alist '(("xps" . 110)
                                  ("dsk" . 110)
                                  ("lat" . 115))
  "Set text-hight for each machine-- default will be 100")

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
  (if (member system-name jmn-pureplus-systems)
      (defconst jmn-pure "plus" "Indicating we are using pure config with plus packages")
    (defconst jmn-pure t "Indicating if we are pure, not using any packages")))

;; flag
(defconst jmn-term (not (display-graphic-p (selected-frame)))
  "Indicating if emacs is being run within a terminal or not")

(if jmn-pure
    (progn
      (defmacro use-package (&rest _))  ;; define use-package macro to do nothing
      (if (string= jmn-pure "plus") ;; turn on all of the plus modes
          (progn (require 'prescient)
                 (dolist (pluslist '(which-key-mode undo-tree-mode ws-butler-mode prescient-persist-mode ivy-mode ivy-rich-mode ivy-prescient-mode counsel-mode))
                   (funcall pluslist 1)))))
  (progn  ;; Connect to the internet and use use-package macros to config below
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
;; This modifies too much and makes git/diffs difficult-- need to find better solution
;; (if jmn-pure
;;     (add-hook 'before-save-hook 'whitespace-cleanup)); non-pure uses ws-butler

(unless (eq system-type 'windows-nt)
  (add-hook 'text-mode-hook 'flyspell-mode))	; enable spellcheck on text mode

;; The following help syncing
(global-auto-revert-mode 1)			; refresh buffer if changed on disk
(setq auto-revert-use-notify nil)		; don't notify?
(setq auto-revert-verbose nil)			;

;; Buffer Menu
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

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
(if jmn-connected-extras
    (use-package all-the-icons
      :defer 1
      :init
      (when (and (not (member "all-the-icons" (font-family-list))) ;; autoinstall fonts
                 (window-system))
        (all-the-icons-install-fonts t))))

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
(global-set-key (kbd "<f5>") 'compile)
;; See recentfiles
(global-set-key (kbd "<f6>") 'recentf-open-files)

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
;; (add-hook 'dired-mode-hook 'dired-hide-details-mode) ;; hide default -- '(' to toggle
(add-hook 'dired-mode-hook 'hl-line-mode)

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
                                       ("\\.odt\\'" "libreoffice")
                                       ("\\.docx\\'" "libreoffice")
                                       ("\\.mp3\\'" "rhythmbox")
                                       ("\\.html\\'" "firefox")
                                       ("\\.epub\\'" "ebook-viewer")
                                       ("\\.pdf\\'" "evince")
                                       ("\\.ipynb\\'" "code")
                                       ("\\.py\\'" "python")
                                       ("\\.sh\\'" "bash"))))


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
(if jmn-connected-extras
    (use-package all-the-icons-dired
      :after dired
      :defer t
      :hook (dired-mode . all-the-icons-dired-mode)))

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

(jmn-set-font-height (alist-get (system-name) jmn-font-height-alist
                                100 nil 'string=)) ;; default is 100

(setq text-scale-mode-step 1.05)

(defun jmn-load-init ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun jmn-vscode-current-buffer-file-at-point ()
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

  (defun jmn-vscode-current-project-root ()
    (interactive)
    (start-process-shell-command "code" nil (concat "code -r " (project-root (project-current t)))))

  (defun jmn-vscode-current-buffer-in-project-root ()
    (interactive)
    (start-process-shell-command "code" nil (concat "code -r " (project-root (project-current t))
                                                    " --goto "
                                                    (buffer-file-name)
                                                    ":"
                                                    (number-to-string (line-number-at-pos))
                                                    ":"
                                                    ;; +1 who knows why
                                                    (number-to-string (+ 1 (current-column))))))

(define-key global-map (kbd "<f12>")
            'jmn-vscode-current-buffer-in-project-root)

(defun jmn-remove-M-1-9-from-mode-map (modemap)
  "remove 'M-[d]' keybinding from a mode-map"
  (dolist (num '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (define-key modemap (kbd (concat "M-" num)) nil)))

(defun remove-function-sexps (functionName)
     (save-excursion
       (goto-char (point-min))
       ;; break up the string so it is not replaced here
       (while (re-search-forward (concat "(" functionName) nil t)
         (backward-sexp)
         (backward-char)
         (kill-sexp))))

(defun remove-if-var-sexps (varName)
     (save-excursion
       (goto-char (point-min))
       ;; break up the string so it is not replaced here
       (while (re-search-forward (concat "(if " varName) nil t)
         (backward-sexp)
         (backward-sexp)
         (backward-char)
         (kill-sexp))))

 (defun purify-my-config-and-save ()
   (interactive)
   (find-file "~/.emacs.d/init.el")
   (remove-function-sexps "use-package")
   (remove-if-var-sexps "jmn-connected-extras")
   (set-visited-file-name "~/.emacs.d/pure_init.el")
   (save-buffer)
   (kill-current-buffer))

(if jmn-term
    (progn
      (xterm-mouse-mode 1)
      (setopt mode-line-end-spaces nil)  ;; Only matters for jmn-pure, doom-modeline is uneffected
      (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))))

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
    (org-mode . (lambda () (rainbow-delimiters-mode 0)))))

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
         ;; (text-mode . git-gutter-mode) ;; effects org-ellipsis
         )
  :config (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (set-face-background 'git-gutter-fr:deleted  (face-background 'default))
  (set-face-foreground 'git-gutter-fr:deleted  "red"))

(if jmn-connected-extras
    (use-package highlight-indent-guides
      :defer 2
      :hook prog-mode
      :config
      (setq highlight-indent-guides-method 'fill)
      (setq highlight-indent-guides-auto-odd-face-perc -7)
      (setq highlight-indent-guides-auto-even-face-perc 7)))

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
  (treemacs-project-follow-mode t) ;; open root of selected file in treemacs
  (setq treemacs-width 30))

(if jmn-connected-extras
    (use-package eglot
      :defer 1
      :ensure-system-package
      ((bash-language-server . "sudo dnf install -y nodejs-bash-language-server")
       (pyright . "pip install pyright")
       (clangd . "sudo dnf install -y clang-tools-extra")
       (texlab . "cargo install --git https://github.com/latex-lsp/texlab --locked --tag v5.22.0") ;; ensure texlab is added to bash PATH
       )
      :hook ((sh-mode . eglot-ensure)
             (python-mode . eglot-ensure)
             (c-mode . eglot-ensure)
             (c++-mode . eglot-ensure)
             (latex-mode . eglot-ensure))
      :config
      ;; make shorter ones later?
      ;; use "M-." for xref-find-definitions
      ;; use "M-?" for xref-find-references
      ;; use "M-g n" and "M-g p" to go to next and previous location
      ;; use ... for xref-pop-marker-stack
      (define-prefix-command 'eglot-prefix)
      (global-set-key (kbd "C-x e") 'eglot-prefix)
      (define-key eglot-prefix (kbd "r") 'eglot-rename)
      (define-key eglot-prefix (kbd "o") 'eglot-code-action-organize-imports)
      (define-key eglot-prefix (kbd "h") 'eldoc)
      (define-key eglot-prefix (kbd "d") 'eglot-find-declaration)
      (define-key eglot-prefix (kbd "a") 'eglot-format-buffer)
      (define-key eglot-prefix (kbd "n") 'flymake-goto-next-error)
      (define-key eglot-prefix (kbd "p") 'flymake-goto-prev-error)
      (define-key eglot-prefix (kbd "b") 'flymake-show-buffer-diagnostics)))

(use-package cmake-mode
  :defer t
  :ensure-system-package ((cmake . "sudo dnf install -y cmake"))
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :ensure t
  :after cmake-mode
  :config (cmake-font-lock-activate))

(use-package cmake-project
  :defer 2
  ;; :hook ((c++-mode . cmake-project-mode )
  ;;        (c-mode . cmake-project-mode))
  )

(if jmn-connected-extras
    (use-package yasnippet
      :defer 1
      :config
      (yas-global-mode 1))

  (use-package yasnippet-snippets
    :after yas-minor-mode))  ;; load basic snippets from melpa

;; free up C-; for evil-nerd-commenter
(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil))
   ;; (progn
   ;;   (define-key flyspell-mode-map (kbd "C-;") nil)
   ;;   (define-key flyspell-mode-map (kbd "M-;") flyspell-auto-correct-previous-word)))

(use-package evil-nerd-commenter
:bind ("C-;". evilnc-comment-or-uncomment-lines))

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

(use-package toml-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package devdocs
 :defer 2
 :bind  ("C-h D" . 'devdocs-lookup))

(defun jmn-smerge-accept-all-lower()
  "Accept all lower changes below the cursor point"
  (interactive)
  (while (not (smerge-next))
    (smerge-keep-lower)))

(defun jmn-smerge-accept-all-upper()
  "Accept all upper changes below the cursor point"
  (interactive)
  (while (not (smerge-next))
    (smerge-keep-upper)))

(with-eval-after-load 'smerge
  (define-key smerge-mode-map (kbd "C-c ^ L") 'jmn-smerge-accept-all-lower)
  (define-key smerge-mode-map (kbd "C-c ^ U") 'jmn-smerge-accept-all-upper))

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

(with-eval-after-load 'python
  ;; if emacs is not run from a terminal, we need to add the PYTHONPATH set in .bashrc
  (unless (getenv "PYTHONPATH")
    (setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'")))
  ;; allow completion
  (setq python-shell-completion-native-enable 1)
  ;; keybindings
  (define-key python-mode-map (kbd "C-RET") 'python-shell-send-statement)
  ;; use ipython for interpreter if it exists
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

;;settings
(setq-default c-basic-offset 2)

;; gdb
(with-eval-after-load 'gdb-mi
  (setq gdb-many-windows t
        gdb-show-main t))

(advice-add 'gdb :after (lambda (&rest r) (tool-bar-mode 1)))

;; mode
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
  :hook ((markdown-mode-hook .  flyspell-mode)
         (markdown-mode-hook .  visual-line-mode)))

(use-package  markdown-preview-mode
  :defer t)

(defun jmn/org-mode-setup ()
  (unless (or jmn-term (version<=  emacs-version "26.1"))
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
    (progn (setq org-ellipsis " ▾")
           (set-face-underline 'org-ellipsis nil)))
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

(unless (or jmn-term (not jmn-connected-extras))
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

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((shell  . t)
                                         (python . t)
                                         (makefile . t)
                                         (latex  . t)
                                         (C      . t)))))

(setq org-confirm-babel-evaluate nil)

(with-eval-after-load 'org

  (if (version<= "27" emacs-version)
      (progn
        (require 'org-tempo) ;; needed for <sh to complete on new systems -- works without on v 26.1
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
    (progn
      (add-to-list 'org-structure-template-alist '("m" "#+BEGIN_SRC makefile
?
#+END_SRC"))
      (add-to-list 'org-structure-template-alist '("js" "#+BEGIN_SRC js
?
#+END_SRC"))
      (add-to-list 'org-structure-template-alist '("sh" "#+BEGIN_SRC shell
?
#+END_SRC"))
      (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp
?
#+END_SRC"))
      (add-to-list 'org-structure-template-alist '("py" "#+BEGIN_SRC python
?
#+END_SRC"))
      (add-to-list 'org-structure-template-alist '("cpp" "#+BEGIN_SRC C++ :includes <iostream>
?
#+END_SRC"))
      (add-to-list 'org-structure-template-alist '("cppnm" "#+BEGIN_SRC C++ :main no
?
#+END_SRC")))))

(with-eval-after-load 'org
  (defconst jmn-latex-scale 1.1 "scaling factor for latex fragments")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale jmn-latex-scale)))

(unless jmn-pure
  (use-package org-fragtog
    :hook
    (org-mode . org-fragtog-mode)))

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

(require 'find-lisp) ;; may not be needed"\\.\\(org\\|org_archived\\)$"

; use "\.org$" or "\\.\\(org\\|org_archived\\)$" exclude or include archives
(setq org-agenda-files (find-lisp-find-files jmn-gtd-directory
                                              ".*\\.\\(org\\|org_archive\\)$"))               ;; "\\.\\(org\\|org_archive\\)$"))

;; level/maxlevel = order in hierarchy
(setq org-refile-targets
      '(("projects.org" :maxlevel . 2)
        ("someday.org" :maxlevel . 1)
        ("whip.org" :level . 1)
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
                   (org-deadline-warning-days 7)))

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

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; org habit;;
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit))
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
  :bind (("C-`". vterm)  ;; gets overwriteent by vterm-toggle
         (:map vterm-mode-map ("C-o" . other-window)))
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
      (set-face-background 'org-block (alist-get 'org-block props))))

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
  (with-eval-after-load 'org
    (jmn-set-gruv-org-faces '((done-color . "gray35" )))))

(defun jmn-load-gruvbox-dark-hard ()
  "Theme for very dark time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-dark-hard t)

  ;; (set-face-foreground 'default "bisque2") ;; default "#ebdbb2"
  ;; (set-face-foreground 'font-lock-comment-face  "#98be65") ;; default "#ebdbb2"
  ;; (set-face-foreground 'font-lock-string-face  "LightGoldenrod3")


  (set-face-background 'mode-line-inactive "gray22")
  (set-face-background 'mode-line-active   "gray35")

  (set-face-background 'line-number
                       (face-attribute 'default :background))
  (set-face-background 'fringe
                       (face-attribute 'default :background))

  (with-eval-after-load 'org
    (jmn-set-gruv-org-faces '((done-color . "gray35" )
                              (org-block . "gray11")))))

(defun jmn-load-gruvbox-dark-hardest ()
  "Theme for very darkest of times"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-dark-hard t)

  (set-face-background 'default "gray7")
  (set-face-foreground 'link  "#83a598") ;; 'tree-sitter-hl-face:function.call inherits

  (set-face-background 'line-number
                       (face-attribute 'default :background))
  (set-face-background 'fringe
                       (face-attribute 'default :background))

  (with-eval-after-load 'tree-sitter-hl
    (set-face-foreground 'tree-sitter-hl-face:variable "#FFA500") ;; was blue, set to aqua
    (set-face-foreground 'tree-sitter-hl-face:variable.parameter
                         (face-attribute 'default :foreground))
    (set-face-foreground 'tree-sitter-hl-face:label  "#d65d0e"))

  (with-eval-after-load 'org
    (jmn-set-gruv-org-faces '(
                              (done-color . "gray35" )
                              (org-block . "gray3")))))

(defun jmn-load-gruvbox-light-medium()
  "Theme for light time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-light-medium t)
  (with-eval-after-load 'org
    (jmn-set-gruv-org-faces '((done-color . "Navajowhite3" )))))

(defun jmn-load-gruvbox-light-hard()
  "Theme for very light time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-light-hard t)
  (with-eval-after-load 'org
    (jmn-set-gruv-org-faces '((done-color . "Navajowhite3" )
                              (org-block . "#fbf1c7"))))) ;; default "#f9f5d7"

(defun jmn-load-gruvbox-light-soft()
  "Theme for very light time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-light-soft t)
  (with-eval-after-load 'org
    (jmn-set-gruv-org-faces '((done-color . "Navajowhite3" )
                              (org-block . "#ebdbb2"))))) ;; default "#f9f5d7"

(defun jmn-set-tab-background-to-default ()
  (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive))
    (set-face-attribute  face nil
                         :foreground (face-foreground 'default)
                         :background (face-background 'default)
                         :weight 'regular)))

(defun jmn-load-pure-dark-theme()
  (load-theme 'wombat)
  (with-eval-after-load 'org
    (set-face-background 'org-block "gray10")
    (setq org-todo-keyword-faces
          `(("TODO" . "Pink")
            ("NEXT" . "gold2")
            ("HOLD" . "orange3")
            ("IGNORE" . "#99968b"))))
  (with-eval-after-load 'tab-bar
    (jmn-set-tab-background-to-default)
    (set-face-background 'tab-bar-tab "gray25")))

(defun jmn-load-pure-light-theme()
  (with-eval-after-load 'org
    (set-face-background 'org-block "gray93")
    (setq org-todo-keyword-faces
          `(("TODO" . "Red1")
            ("NEXT" . "orange")
            ("HOLD" . "Black")
            ("IGNORE" . "gray60"))))
  (with-eval-after-load 'tab-bar
    (jmn-set-tab-background-to-default)
    (set-face-background 'tab-bar-tab "darkseagreen2")))

(if (window-system)
    (set-frame-height (selected-frame) 53))

(if jmn-dark-mode
    (if (eq system-type 'gnu/linux)
        (if jmn-pure
            (jmn-load-pure-dark-theme)  ;; dark, linux, pure
          (jmn-load-gruvbox-dark-hardest))  ;; dark, linux, connected
      (jmn-load-pure-light-theme))  ;; dark, windows
  (jmn-load-pure-light-theme))  ;; non-dark

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

(if (version<= "29" emacs-version)
    (progn
      (defun toggle-full-screen-with-transparency ()
        "Toggle full screen and adjust frame transparency."
        (interactive)
        (let ((current-alpha (frame-parameter nil 'alpha-background)))
          (transparency 100)
          (toggle-frame-fullscreen)
          (sit-for 0.7)
          (transparency current-alpha)
          ))
      ;; (global-unset-key (kbd "<f11>"))
      ;; (global-set-key (kbd "<f11>") 'toggle-full-screen-with-transparency)
      ))

(defun jmn-set-background-unspecified ()
  "Set background of buffer and line numbers to unspecified"
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-background 'line-number "unspecified-bg"))

(if jmn-term
    (add-hook 'window-setup-hook 'jmn-set-background-unspecified))

(setq org-preview-latex-process-alist
      '((dvipng :programs
                ("latex" "dvipng")
                :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                (1.0 . 1.0)
                :latex-compiler
                ("latex -interaction nonstopmode -output-directory %o %f")
                :image-converter
                ("dvipng -D %D -T tight -o %O %F")
                :transparent-image-converter
                ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
        (dvisvgm :programs
                 ("latex" "dvisvgm")
                 :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                 (1.7 . 1.5)
                 :latex-compiler
                 ("latex -interaction nonstopmode -output-directory %o %f")
                 :image-converter
                 ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O"))
        (imagemagick :programs
                     ("latex" "convert")
                     :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                     (1.0 . 1.0)
                     :latex-compiler
                     ("pdflatex -interaction nonstopmode -output-directory %o %f")
                     :image-converter
                     ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(setq org-image-actual-width 600)

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

(if (or jmn-pure (not jmn-connected-extras))
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
      (tab-bar-close-other-tabs) ; ensures (tab-bar-mode 1) works on older systems

      (defun jmn-create-my-tabs()
        "Create my default tabs"
        (interactive)
        (tab-close-other)
        (delete-other-windows)
        ;; TAB 1
        (tab-bar-rename-tab "gtd")
        (find-file (concat jmn-gtd-directory "projects.org"))
        (jmn-agenda)

        ;; TAB 2
        (tab-bar-new-tab)
        (tab-bar-rename-tab "workspace")
        (dired "/home/ape/Programming/projects/radar/radar-signal-processing")
        (unless jmn-pure
          (magit-status))

        ;; ;; TAB
        ;; (tab-bar-new-tab)
        ;; (if jmn-pure
        ;;     (progn
        ;;       (term "/bin/bash")
        ;;       (tab-bar-rename-tab "term"))
        ;;   (progn
        ;;     (vterm)
        ;;     (tab-bar-rename-tab "vterm")))
        ;; (delete-other-windows)

        ;; TAB N
        (tab-bar-new-tab)
        (tab-bar-rename-tab "config")
        (find-file jmn-config-location)
        (unless jmn-pure
          (magit-status))

        (tab-bar-select-tab 2))))

(if jmn-connected-extras
    (use-package dashboard
      :ensure t
      :init (dashboard-setup-startup-hook)
      :bind ("C-c d" . dashboard-open)
      :config
      (setq dashboard-banner-logo-title "Habits, not goals.")

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
      (setq dashboard-set-footer nil)
      (setq dashboard-startupify-list
            '(dashboard-insert-banner
              dashboard-insert-newline
              dashboard-insert-banner-title
              dashboard-insert-newline
              dashboard-insert-init-info
              dashboard-insert-items
              dashboard-insert-newline
              ;;dashboard-insert-footer
              )))

  (defun my-dashboard-hook()
    "Needed to define these after hook for some reason"
    (define-key dashboard-mode-map (kbd "n")  'dashboard-next-line)
    (define-key dashboard-mode-map (kbd "p")  'dashboard-previous-line))

  (add-hook 'dashboard-mode-hook 'my-dashboard-hook)
  (add-hook 'dashboard-mode-hook 'hl-line-mode))

(unless jmn-pure
(use-package hnreader))

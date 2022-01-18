(setq comp-async-report-warnings-errors nil)

(add-to-list 'load-path "~/.dotfiles/.emacs.d/elpa/gcmh-20201116.2251")
(gcmh-mode 1)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; no need for :ensure t for each package.
(setq use-package-verbose t) ; log configure/loading messages in *Messages*

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq inhibit-startup-message t)           ; inhibit startup message
(tool-bar-mode -1)                         ; remove toolbar
(menu-bar-mode -1)                         ; Disable the menu bar
(scroll-bar-mode -1)                       ; remove side scrollbar
(tooltip-mode -1)                          ; Disable tooltips
(set-fringe-mode 10)                       ; Give some breathing room
(add-hook 'text-mode-hook 'flyspell-mode)  ; enable spellcheck on text mode

(setq visible-bell t)                      ; Set up the visible bell

;; Open text files in Org-Mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode)) 
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(use-package emacs
  :custom
  ;; Fully redraw the display before it processes queued input events.
  (redisplay-dont-pause            t)
  ;; Number of lines of continuity to retain when scrolling by full screens
  (next-screen-context-lines       2)
  (scroll-conservatively       10000) ;; only 'jump' when moving this far off the screen
  (scroll-step                     1) ;; Keyboard scroll one line at a time
  (mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
  (mouse-wheel-follow-mouse        t) ;; Scroll window under mouse
  (fast-but-imprecise-scrolling    t) ;; No (less) lag while scrolling lots.
  (auto-window-vscroll           nil) ;; Cursor move faster
  )

(use-package undo-tree
  :defer 2
  :config
  (global-undo-tree-mode 1))

(setq make-backup-files nil)               ; stop creating backup~ files
(setq auto-save-default nil)               ; stop creating #autosave# files

(dolist (mode '(org-mode-hook
                 term-mode-hook
                 shell-mode-hook
                 treemacs-mode-hook
                 eshell-mode-hook
                 vterm-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-display-line-numbers-mode t)     ; Puts line numbers on ALL buffers

(use-package gruvbox-theme
    :init (load-theme 'gruvbox-dark-hard t))

(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(transparency 94)  ;; Default value generally e [94,96]

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

(use-package ivy
  :delight ivy-mode
  :config
  (ivy-mode 1)
  ;; remove ^ on the inputbuffer
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :after ivy
  :init  
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)      ; displays ivy-rich info in minibuffer
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         ))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package which-key
  :defer 0
  :delight which-key-mode  
  :config(which-key-mode)
  (setq which-key-idle-delay 0.8))

(use-package lsp-treemacs
  :after lsp)

(use-package helpful
:commands (helpful-callable helpful-variavle helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-o") 'other-window)
;; (global-set-key (kbd "M-SPC") 'other-window)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :delight smartparens-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status)
  :custom
  ;display Magit status buffer in the same buffer rather than splitting it. 
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile
  :after lsp
  ;; :delight projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile-mode
  :config (counsel-projectile-mode))

(use-package lsp-mode
  :delight lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; or "C-l"
  :custom ((lsp-idle-delay 0.2))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode) ; for elpy
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp)

(use-package company
  :after lsp
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :bind (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common)))

(use-package company-box
  :delight company-box-mode 
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :defer 2
  :after company
  :config
  (company-prescient-mode +1))

(use-package yasnippet
  :delight( yas-minor-mode)
  :after lsp)

(use-package yasnippet-snippets
  :after yas-minor-mode) ; load basic snippets from melpa

(use-package flycheck
  :diminish flycheck-mode
  :after lsp)

(use-package dap-mode
  :commands dap-mode)

(use-package evil-nerd-commenter
:bind ("M-;". evilnc-comment-or-uncomment-lines))

(use-package smart-compile
  :commands smart-compile)

(setq-default c-basic-offset 4)

(defun my-c-c++-mode-hook-fn ()
  (lsp)                ; turn on
  (smart-compile)
  (smartparens-mode)   ; turn on
  (local-set-key (kbd "<tab>") #'company-indent-or-complete-common) ;tab comp
  (yas-minor-mode-on)  ; turn on
  (abbrev-mode -1)        ; turn off
  )
(add-hook 'c-mode-hook #'my-c-c++-mode-hook-fn)
(add-hook 'c++-mode-hook #'my-c-c++-mode-hook-fn)

; npm must be installed on the system.
  (use-package lsp-pyright
    :after lsp
    :hook (python-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp))))  ; or lsp-deferred

;; configure pythong-mode
(use-package python-mode
  :ensure nil ; don't install, use the pre-installed version

  :custom
  (python-shell-completion-native-enable 1)
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
                                        ; this command doesn't work BUT without, python-mode "won't load".
  :bind (:map python-mode-map ("C-RET" . python-shell-send-statement))
  )

(defun my-python-mode-hook-fn ()
  (lsp)
  (company-mode 1)
  (smartparens-mode)
  (local-set-key (kbd "<tab>") #'company-indent-or-complete-common))

(add-hook 'python-mode-hook #'my-python-mode-hook-fn)

(defun jmn/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (rainbow-delimiters-mode 1)
  (projectile-mode -1)
  ;; edit the modeline-- not needed for doom-modeline
  ;; (diminish 'visual-line-mode)
  ;; (diminish 'flyspell-mode)
  ;; (diminish 'org-indent-mode)
  ;; (diminish 'buffer-face-mode)
  ;; (diminish 'yas-minor-mode)
  ;; (diminish 'eldoc-mode)
  )

(defun jmn/org-font-setup ()

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                     '(("^ *\\([-]\\) "
                      (0 (prog1 () (compose-region (match-beginning 1)
                                                   (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell"
                        :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
                      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . jmn/org-mode-setup)
  :config
  (jmn/org-font-setup)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil
        org-image-actual-width nil) ; fix to allow picture resizing
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                '((python . t)
                                  (latex  . t)
                                  (C      . t))))

(setq org-confirm-babel-evaluate nil)

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("la" . "src latex"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python  :results output"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++  :includes <iostream>"))
  (add-to-list 'org-structure-template-alist '("cppnm" . "src C++  :main no"))
)

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :bind (:map vterm-mode-map ("C-o" . other-window))
  :config
  ;(setq term-prompt-regexp "^[^$]*[$] *");; match your custom shell
;;(setq vterm-shell "zsh");; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package dired
   :ensure nil
   :commands dired
   :custom  (setq dired-listing-switches "-agho --group-directories-first"))

 (use-package treemacs-icons-dired
   :after dired
   :config (treemacs-icons-dired-mode) )

;A rather janky mode which lists the recursive size of each foler/item in dired. 
 (use-package dired-du
   :commands du)

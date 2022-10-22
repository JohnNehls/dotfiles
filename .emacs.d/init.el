;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

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
;; (menu-bar-mode -1)                         ; Disable the menu bar
(scroll-bar-mode -1)                       ; remove side scrollbar
(tooltip-mode -1)                          ; Disable tooltips
(set-fringe-mode 10)                       ; Give some breathing room
(setq visible-bell t)                      ; Set up the visible bell

(add-hook 'text-mode-hook 'flyspell-mode)  ; enable spellcheck on text mode
;; (add-hook 'prog-mode-hook 'hl-line-mode)   ; highlight lines when programming

;; Open text files in Org-Mode
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(use-package emacs
  :custom
  ;; Fully redraw the display before it processes queued input events.
  (redisplay-dont-pause            t)

  ;; Number of lines of continuity to retain when scrolling by full screens
  ;; (next-screen-context-lines       2)  ;; golden ration pkg replaced this

  ;; only 'jump' when moving this far off the screen
  (scroll-conservatively       10000)
  (scroll-step                     1) ;; Keyboard scroll one line at a time
  (mouse-wheel-progressive-speed nil) ;; Don't accelerate scrolling
  (mouse-wheel-follow-mouse        t) ;; Scroll window under mouse
  (fast-but-imprecise-scrolling    t) ;; No (less) lag while scrolling lots.
  (auto-window-vscroll           nil) ;; Cursor move faster
  (pixel-scroll-precision-mode     1) ;; pixel based scrolling
  )

(use-package fast-scroll
  :ensure t
  :demand t
  :config
  (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
  (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
  (fast-scroll-config)
  (fast-scroll-mode 1))

(use-package golden-ratio-scroll-screen
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

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

(use-package monokai-theme
    :init (load-theme 'monokai t))
;; Saving my SECOND favorite theme which is easier on the eyes.
;; (use-package gruvbox-theme
;;     :init (load-theme 'gruvbox-dark-hard t))

(use-package all-the-icons)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(transparency 96)  ;; Default value generally e [94,96]

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(recentf-mode 1) ;; needed for recent files in dashboard

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content 1)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 7)
                          ;; (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)

  (dashboard-modify-heading-icons '((recents . "file-text")))


  (setq dashboard-set-footer nil)
  )

(use-package goto-last-change
  :ensure t
  :bind ("C-;" . goto-last-change))
  ;; :hook (org-mode . goto-last-change))

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

;; Make font bigger/smaller.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

  ;; (global-unset-key (kbd "C-<SPC>"))
  ;; (global-unset-key (kbd "C-m"))
  ;; (global-set-key (kbd "C-m") 'set-mark-command)
  ;; (global-set-key (kbd "C-<SPC>") 'other-window)
  ;; (global-set-key (kbd "M-SPC") 'other-window)

;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

(show-paren-mode    1) ; Highlight parentheses pairs.
;; (electric-pair-mode 1) ; Close pairs automatically.

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

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.5)
  ;; (global-set-key (kbd "C-<tab>") 'company-complete)
)
(global-company-mode 1)

(use-package company-box
  :delight company-box-mode
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :defer 2
  :after company
  :config
  (company-prescient-mode +1))

(use-package lsp-mode
  :delight lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; or "C-l"
  :custom ((lsp-idle-delay 0.5)) ;; 0.5 is the defualt
  :config
  (lsp-enable-which-key-integration t)
  ;; Annoying stuff (uncomment to turn off)
  (setq lsp-enable-links nil)
  ;; (setq lsp-signature-render-documentation nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-completion-enable-additional-text-edit nil)


  ;; `-background-index' requires clangd v8+!
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode) ; for elpy
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp)

(use-package yasnippet
  :delight( yas-minor-mode)
  :after lsp)

(use-package yasnippet-snippets
  :after yas-minor-mode) ; load basic snippets from melpa

(yas-global-mode 1)

(use-package flycheck
  :diminish flycheck-mode
  :after lsp)

(use-package dap-mode
  :commands dap-mode)

(use-package evil-nerd-commenter
:bind ("M-;". evilnc-comment-or-uncomment-lines))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
:ensure t
:after cmake-mode
:config (cmake-font-lock-activate))

(use-package cmake-project
  :hook ((c++-mode . cmake-project-mode )
         (c-mode . cmake-project-mode))
  )

(setq compilation-scroll-output t)

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

(defun my-c-c++-mode-hook-fn ()
  (lsp)                ; turn on
  (local-set-key (kbd "C-<tab>") #'lsp-format-buffer) ;tab comp
  (smartparens-mode 1)
  )

(add-hook 'c-mode-hook #'my-c-c++-mode-hook-fn)
(add-hook 'c++-mode-hook #'my-c-c++-mode-hook-fn)

(use-package pyvenv
:ensure t
:defer t
:diminish
:config

(setenv "WORKON_HOME" "/home/ape/.conda/envs")
        ; Show python venv name in modeline
        (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
        (pyvenv-mode t))

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
  ;; (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  )

(add-hook 'python-mode-hook #'my-python-mode-hook-fn)

(defun jmn/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (rainbow-delimiters-mode 0)
  (projectile-mode -1)
  ;; (company-mode 1)
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
                                              '((shell  . t)
                                                (python . t)
                                                (latex  . t)
                                                (C      . t))))

              (setq org-confirm-babel-evaluate nil)

              (with-eval-after-load 'org
                ;; This is needed as of Org 9.2
                (require 'org-tempo)
                (add-to-list 'org-structure-template-alist '("la" . "src latex"))
                (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
                (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
                (add-to-list 'org-structure-template-alist '("py" . "src python  :results output"))
                (add-to-list 'org-structure-template-alist '("pyim" . "src python :results file :var f=REPLACE
import matplotlib.pyplot as plt
plt.savefig(f)
return f"))
                (add-to-list 'org-structure-template-alist '("cpp" . "src C++  :includes <iostream>"))
                (add-to-list 'org-structure-template-alist '("cppnm" . "src C++  :main no")))

(defconst jmn-latex-scale 1.0 "scaling factor for latex fragments")
(setq org-format-latex-options (plist-put org-format-latex-options :scale jmn-latex-scale))

(defun update-org-latex-fragments ()
  (org-latex-preview '(64))
  (plist-put org-format-latex-options :scale (+ jmn-latex-scale  (* 0.3 text-scale-mode-amount)))
  (org-latex-preview '(16)))
(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; (use-package jupyter
;;     :after (org)
;;     ;; :straight t
;;     )

;; (org-babel-do-load-languages 'org-babel-load-languages
;;                              (append org-babel-load-languages
;;                                      '((jupyter . t))))

(use-package ein
  :commands (ein:notebooklist-open)
  ;; :config
  ;; (require 'ein-loaddefs)
  ;; (require 'ein)
  ;; (define-key ein:notebook-mode-map (kbd "<C-tab>") 'my-function)
  )

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :bind (:map vterm-mode-map ("C-o" . other-window))
  :config
  ;;(setq term-prompt-regexp "^[^$]*[$] *");; match your custom shell
  ;;(setq vterm-shell "zsh");; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  )

;; (global-unset-key (kbd "C-t"))`
(global-set-key (kbd "C-`") 'vterm-toggle)

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

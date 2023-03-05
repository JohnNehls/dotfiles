;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Dropbox/.dotfiles/emacs.org"))
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

(use-package use-package-ensure-system-package
:ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 30)
  (auto-package-update-prompt-before-update t)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(setq inhibit-startup-message t)           ; inhibit startup message
(tool-bar-mode -1)                         ; remove toolbar
(menu-bar-mode -1)                         ; Disable the menu bar
(scroll-bar-mode -1)                       ; remove side scrollbar
(tooltip-mode -1)                          ; Disable tooltips
(set-fringe-mode 10)                       ; Give some breathing room
(setq visible-bell t)                      ; Set up the visible bell
(save-place-mode 1)                        ; Open file where last visited
(add-hook 'text-mode-hook 'flyspell-mode)  ; enable spellcheck on text mode
(setq Buffer-menu-name-width 35)           ; give name more room
;; (add-hook 'prog-mode-hook 'hl-line-mode)   ; highlight lines when programming

;; The following helps syncing
(global-auto-revert-mode 1)                ; refresh buffer if changed on disk
(setq auto-revert-use-notify nil)          ; don't notify?
(setq auto-revert-verbose nil)             ;

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

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil) ;; don't save ~undo-tree~ file
  )

(setq backup-directory-alist
      '( ("." . "~/.dotfiles/.emacs.d/filebackups")))

(use-package all-the-icons
:init
(when (and (not (member "all-the-icons" (font-family-list))) ;; autoinstall fonts
           (window-system))
  (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (if (version<= "29" emacs-version)
      (set-frame-parameter nil 'alpha-background value)
    (set-frame-parameter (selected-frame) 'alpha value)))

(use-package ws-butler
  :defer t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(recentf-mode 1) ;; needed for recent files in dashboard

(use-package dashboard
  :ensure t

  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 1)
  (setq dashboard-center-content 1)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 12)
                          ;; (bookmarks . 5)
                          (projects . 4)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'project-el)

  (dashboard-modify-heading-icons '((recents . "file-text")))

  (setq dashboard-set-footer nil)

  :bind ( :map dashboard-mode-map
          ;; ("j"  .  nil)
          ;; ("k"  .  nil)
          ("n"  .  'dashboard-next-line)
          ("p"  .  'dashboard-previous-line)))

(use-package dired
  :ensure nil
  :commands dired
  ;; :hook (dired-mode . dired-hide-details-mode) ;; '(' to toggle details
  :config
  (setq dired-listing-switches "-agho --group-directories-first" )
  (setq find-ls-option '("-print0 | xargs -0 ls -agho" . ""))
  (setq dired-dwim-target t) ;; guess other dired directory for copy and rename
  (setq wdired-allow-to-change-permissions t)
  (define-key dired-mode-map (kbd "C-o") 'other-window)
  (setq dired-guess-shell-alist-user '(("\\.png\\'" "shotwell")
                                       ("\\.jpeg\\'" "shotwell")
                                       ("\\.mp4\\'" "vlc")
                                       ("\\.mp3\\'" "rhythmbox")
                                       ("\\.html\\'" "firefox")
                                       ("\\.epub\\'" "ebook-viewer")
                                       ("\\.pdf\\'" "evince")
                                       ("\\.ipynb\\'" "code"))))

  ;; nice icons in dired
  (use-package treemacs-icons-dired
    :after dired
    :defer t
    :config (treemacs-icons-dired-mode) )

  ;; janky mode which lists the recursive size of each foler/item in dired.
  (use-package dired-du
    :commands dired-du-mode
    :defer t
    :config (setq dired-du-size-format t))

  ;; use a single dired session
  (use-package dired-single)

  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
          loaded."
    (define-key dired-mode-map [remap dired-find-file]
                'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
                'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory]
                'dired-single-up-directory))

  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (my-dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'my-dired-init))

(defun proced-settings ()
    (proced-toggle-auto-update 5)) ;; auto update every 4 seconds

(add-hook 'proced-mode-hook 'proced-settings)

(setq native-comp-async-report-warnings-errors nil)

(use-package goto-last-change
  :ensure t
  :bind ("C-;" . goto-last-change))

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

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)      ; displays ivy-rich info in minibuffer
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
         ))

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

(use-package which-key
  :defer 0
  :delight which-key-mode  
  :config(which-key-mode)
  (setq which-key-idle-delay 0.8))

;; general improvements
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
 (global-set-key (kbd "C-x C-b") 'buffer-menu)   ;; open buffer menue in current buffer
 (global-set-key (kbd "C-x C-k") 'kill-current-buffer)   ;; "C-x k" asks which buffer
 (global-set-key (kbd "C-o") 'other-window)  ;; default is "C-x o"
 (global-set-key (kbd "M-o") 'previous-multiframe-window)
 (global-set-key (kbd "C-c C-c") 'eval-buffer)
 (global-set-key (kbd "C-c C-r") 'eval-region)

 ;; Make font bigger/smaller.
 (global-set-key (kbd "C-=") 'text-scale-increase)
 (global-set-key (kbd "C--") 'text-scale-decrease)
 (global-set-key (kbd "C-0") 'text-scale-adjust)

 ;; Org notes flow
 (global-set-key (kbd "<f5>") 'org-store-link)
 (global-set-key (kbd "<f6>") 'org-insert-link)
;; (global-set-key (kbd "<f7>") 'org-agenda ;; set later!
(global-set-key (kbd "<f8>") 'org-html-export-to-html)

 ;; writing/editing
 (global-set-key (kbd "<f9>") 'ispell-word)
 (global-set-key (kbd "<f10>") 'dictionary-lookup-definition)

 ;; Buffer-menu-mode
 (define-key Buffer-menu-mode-map (kbd "C-o") 'other-window)
 (define-key Buffer-menu-mode-map (kbd "M-o") 'previous-multiframe-window)
 ;; "o" opens in another buffer and moves focus
 ;; "C-M-o" opens in another buffer and keeps focus in the Buffer-menu
 (define-key Buffer-menu-mode-map (kbd "C-M-o") 'Buffer-menu-switch-other-window)

 ;; compilation-mode
 (define-key compilation-mode-map (kbd "C-o") 'other-window)

 ;; grep-mode
 (defun jmn-grep-keybindings()
   (define-key grep-mode-map (kbd "o") 'compilation-display-error)
   (define-key grep-mode-map (kbd "C-o") 'other-window))

 (add-hook 'grep-mode-hook #'jmn-grep-keybindings)

(set-face-attribute
 'default nil
 :height (assoc-default (system-name) '(("xps" . 115)
                                        ("dsk" . 120))))

(setq text-scale-mode-step 1.05)

(defun jmn-load-init ()
  (interactive)
  (load "~/.dotfiles/.emacs.d/init.el"))

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

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(show-paren-mode    1) ; Highlight parentheses pairs.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status)
  ;; :custom
  ;; ;display Magit status buffer in the same buffer rather than splitting it.
  ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package highlight-indent-guides
  :hook prog-mode
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 50))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.5)
  ;; (global-set-key (kbd "C-<tab>") 'company-complete)
)
(global-company-mode 1)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :defer 2
  :after company
  :config
  (company-prescient-mode +1))

(use-package lsp-mode
  :delight lsp-mode
  :commands (lsp)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; or "C-l"
  :custom ((lsp-idle-delay 0.5)) ;; 0.5 is the defualt
  :config
  (lsp-enable-which-key-integration t)
  ;; Annoying stuff (uncomment to turn off)
  ;; (setq lsp-enable-links nil)
  ;; (setq lsp-signature-render-documentation nil)
  (setq lsp-headerline-breadcrumb-enable nil)
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

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :commands dap-mode)

(use-package flycheck
  :diminish flycheck-mode
  :after lsp)

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp))

(use-package cmake-font-lock
:ensure t
:after cmake-mode
:config (cmake-font-lock-activate))

(use-package cmake-project
  :hook ((c++-mode . cmake-project-mode )
         (c-mode . cmake-project-mode))
  )

(use-package yasnippet
  :delight( yas-minor-mode)
  :after lsp)

(use-package yasnippet-snippets
  :after yas-minor-mode) ; load basic snippets from melpa

(yas-global-mode 1)

(use-package flyspell
  :ensure nil
  :bind (:map flyspell-mode-map ("C-;" . nil)))

(use-package evil-nerd-commenter
:bind ("M-;". evilnc-comment-or-uncomment-lines))

(use-package tree-sitter-langs)
;; add hooks in languages below (1/23 not available for elisp)

; (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(defun my-sh-mode-hook-fn()
  (setq sh-basic-offset 2
        sh-indentation 2) ;; defaults are 4
  (tree-sitter-hl-mode)
  (lsp))

(use-package sh-script
    :ensure-system-package
    ((bash-language-server . "sudo dnf install -y nodejs-bash-language-server"))
    :config
    (setq sh-basic-offset 2
          sh-indentation 2) ;; defaults are 4
    (add-hook 'sh-mode-hook #'my-sh-mode-hook-fn))

(use-package ein
  :commands (ein:notebooklist-open)
  ;; :config
  ;; (require 'ein-loaddefs)
  ;; (require 'ein)
  ;; (define-key ein:notebook-mode-map (kbd "<C-tab>") 'my-function)
  )

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
                            (lsp))))

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
  (require 'dap-python)
  (tree-sitter-hl-mode)
  (jmn-jmn-display-lines-for-long-files)
  ;; (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
  )

(add-hook 'python-mode-hook #'my-python-mode-hook-fn)

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
  ;(smartparens-mode 1) no longer have it?
  (tree-sitter-hl-mode 1))

(add-hook #'c-mode-hook #'my-c-c++-mode-hook-fn)
(add-hook #'c++-mode-hook #'my-c-c++-mode-hook-fn)

(defun jmn/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (rainbow-delimiters-mode 0)
  ;; fix issue where it matches > with partentheses -- may break blocks with <>
  (modify-syntax-entry ?< ".")
  (modify-syntax-entry ?> ".")
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
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)
                  ))
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
        org-edit-src-content-indentation 2 ;; I think I undo this somewhere for de/tangling
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil
        org-list-indent-offset 1
        org-image-actual-width nil ; fix to allow picture resizing
        org-return-follows-link t  ; keep for sure ;@work
        org-use-speed-commands t ; try out
        )
  (setq org-agenda-tags-column
      (assoc-default (system-name) '(("xps" . 75)
                                     ("dsk" . 75)))) ;; default is auto
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
  (plist-put org-format-latex-options :scale (+ jmn-latex-scale  (* 0.3 text-scale-mode-amount)))
  (org-latex-preview '(16)))
(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(defun org-agenda-show-my-dashboard (&optional arg)
  (interactive "P")
  (org-agenda arg "d"))

(global-set-key (kbd "C-c d") #'org-agenda-show-my-dashboard)

(use-package htmlize)

;; Org Agenda ;; (setq org-agenda-window-setup 'reorganize-frame)
;; Exited with ‘q’ or ‘x’ and the old state is restored.
(setq org-agenda-restore-windows-after-quit 1)
(setq org-agenda-span 'day)

;; SOMEDAY itmes are ommitted from GTD interface on purpose
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|"
                                    "DONE(d)")))

(require 'find-lisp)
(setq org-directory "~/Documents/gtd/")

(setq org-agenda-files (find-lisp-find-files org-directory "\.org$"))

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

(setq org-agenda-start-with-log-mode t) ;; allows us to see closed in calendar
(setq org-log-done 'time) ;; creates CLOSED time tag
(setq org-log-into-drawer t) ;; creates a LOGBOOK drawer for notes

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
                 (org-agenda-files (list (concat org-directory "inbox.org")))))

          (todo "NEXT"
                ((org-agenda-overriding-header "In Progress")
                 (org-agenda-files (list (concat org-directory "projects.org")
                                         (concat org-directory "next.org")
                                         (concat org-directory "inbox.org")))))

          (todo "TODO"
                ((org-agenda-overriding-header "Projects")
                 (org-agenda-files (list (concat org-directory  "projects.org")))))

          (todo "TODO"
                ((org-agenda-overriding-header "One-off Tasks")
                 (org-agenda-files (list (concat org-directory  "next.org"))))
                (org-agenda-skip-function '(org-agenda-skip-entry-if
                                            'deadline 'scheduled)))))))

(defun jmn-someday() "Quick access to someday.org (no links in agenda)"
       (interactive)
       (find-file (concat org-directory "someday.org")))

;; remove "C-c a" for something else later?
(global-set-key (kbd "C-c a") (lambda (&optional args)
                                (interactive "P")
                                (org-agenda args " ")))

(global-set-key (kbd "<f7>") (lambda (&optional args)
                                (interactive "P")
                                (org-agenda args " ")))

(setq org-agenda-todo-ignore-scheduled 'all) ;; cant get it to work for deadlines

;; org habit;;
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column
      (assoc-default (system-name) '(("xps" . 56)
                                     ("dsk" . 52)))) ;; default is 40

(setq org-capture-templates
      `(("t" "Todo [inbox]" entry
         (file ,(concat org-directory "inbox.org"))
         "* TODO %i%?" :empty-lines 1)

        ("T" "Todo Today [inbox]" entry
           (file ,(concat org-directory "inbox.org"))
           "* TODO %?\nDEADLINE: %t" :empty-lines 1)

        ("l" "Linked Todo [inbox]" entry
         (file ,(concat org-directory "inbox.org"))
         "* TODO %i%? \n %a" :empty-lines 1)

        ("s" "Schedule" entry
         (file+headline ,(concat org-directory "whip.org")  "Whip")
         "* %i%? \n %U %^t" :empty-lines 1)

        ("j" "Journal" entry
         (file+datetree ,(concat org-directory "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a"  :empty-lines 1)))

(defun org-archive-done-tasks-tree ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'tree))

(defun org-archive-done-tasks-file ()
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

(use-package vterm
  :commands vterm
  :defer t
  :bind (:map vterm-mode-map ("C-o" . other-window))
  :config
  ;;(setq term-prompt-regexp "^[^$]*[$] *");; match your custom shell
  ;;(setq vterm-shell "zsh");; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :after vterm
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

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

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

   (set-face-foreground 'default "gray75") ;; default "#ebdbb2"
  (set-face-foreground 'default "moccasin") ;; default "#ebdbb2"

    (jmn-set-gruv-org-faces '((done-color . "gray35" ))))


(defun jmn-load-gruvbox-dark-hard ()
  "Theme for very dark time"
  (interactive)
  (disable-theme (car custom-enabled-themes))
  (load-theme 'gruvbox-dark-hard t)
  (set-face-background 'line-number
                       (face-attribute 'default :background))

  ;; (set-face-foreground 'default "PeachPuff3") ;; default "#ebdbb2"
  ;; (set-face-foreground 'default "gray75") ;; default "#ebdbb2"
  ;; (set-face-foreground 'default "moccasin") ;; default "#ebdbb2"
  (set-face-foreground 'default "bisque2") ;; default "#ebdbb2"
  (set-face-foreground 'font-lock-comment-face  "#98be65") ;; default "#ebdbb2"
  (set-face-foreground 'font-lock-string-face  "LightGoldenrod3")
  (set-face-foreground 'font-lock-builtin-face  "Orange3")
  (jmn-set-gruv-org-faces '((done-color . "gray35" )
                            (org-block . "#282828"))))


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

(use-package doom-themes)
(defun jmn-load-doom-one()
    "doom-one for dark time"
    (interactive)
    (disable-theme (car custom-enabled-themes))
    (load-theme 'doom-one)
    (with-eval-after-load 'org
      (set-face-foreground 'org-priority (face-foreground font-lock-builtin-face))
      (setq org-todo-keyword-faces
            `(("NEXT" .  ,(face-foreground font-lock-type-face))
              ("HOLD" . ,(face-foreground font-lock-variable-name-face))))))


  (defun jmn-load-doom-one-light()
    "doom-one for light time"
    (interactive)
    (disable-theme (car custom-enabled-themes))
    (load-theme 'doom-one-light)
    (with-eval-after-load 'org
      (setq org-todo-keyword-faces
            `(("NEXT" .  ,(face-foreground font-lock-type-face))
              ("HOLD" . ,(face-foreground font-lock-variable-name-face))))))

(jmn-load-gruvbox-dark-hard)
;; (run-at-time "7:00 am" nil #'jmn-load-doom-one-light)
;; (run-at-time "4:30 pm" nil #'jmn-load-doom-one)

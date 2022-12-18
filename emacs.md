
# Table of Contents

1.  [Setup](#org5db0088)
    1.  [Auto-tangle](#org71261a6)
    2.  [Use-package](#org6603013)
    3.  [Automatic Package Updates](#org5947687)
2.  [General Emacs](#orgf1410de)
    1.  [UI Configurations](#orgd730219)
    2.  [Dired](#orgb91a11c)
    3.  [Native Compilation](#orga80c592)
    4.  [Goto last change](#orga0e082c)
    5.  [Input Buffer, Directory Search](#org2ab0404)
    6.  [Helpful and Which-key](#org8c374b0)
    7.  [Grammarly](#orgf1fd661)
    8.  [Keybindings](#orgc2bcbd0)
    9.  [Font size](#org3903c7e)
3.  [General Development](#orgcb3163e)
    1.  [Flyspell comments](#org8a438b2)
    2.  [Parens/delimiters](#org66744b4)
    3.  [Magit](#orgea12722)
    4.  [Git-Gutter](#orge67c736)
    5.  [Projectile NOT USED](#org340eba9)
    6.  [Company-Mode](#org5f67c80)
    7.  [LSP and DAP](#orgb56cbc8)
    8.  [Flycheck](#orga1b2aa9)
    9.  [CMake](#orgd2874fe)
    10. [Yasnippet](#orgccdfa5e)
    11. [Flyspell](#orgd0c0514)
    12. [Evil nerd commenter](#orge7ec481)
4.  [Emacs-lisp](#org36c6c36)
5.  [Bash](#org17aacc8)
6.  [Python](#orge6ad938)
    1.  [Jupyter Notebooks](#orgf439601)
    2.  [Pyvenv](#orga731adc)
    3.  [Python-mode](#org55a796e)
    4.  [Hook](#orgae1182a)
7.  [C/C++](#org18c8d06)
    1.  [Compilation Buffer](#orgfd662e7)
    2.  [Hook](#org9ad03d2)
8.  [Org-Mode](#org5a2490c)
    1.  [Mode setup](#org7f1b758)
    2.  [Fonts](#org30aae49)
    3.  [Start](#org0f124e3)
    4.  [Bullets](#org6ea41c2)
    5.  [Center column](#orga2e023d)
    6.  [Org-babel](#org2598b9c)
    7.  [Inline latex](#org60d2a63)
    8.  [Keybindings](#org584d8ac)
9.  [Terminals](#org048bb21)
    1.  [vterm](#org9072970)
    2.  [term-mode](#orgef6cfd8)
    3.  [shell-mode](#org4c43bf5)
    4.  [eshell](#org9ec6015)
10. [Localwords](#orga70b75f)



<a id="org5db0088"></a>

# Setup


<a id="org71261a6"></a>

## Auto-tangle

This snippet adds a hook to `org-mode` buffers so that `efs/org-babel-tangle-config` gets executed each time such a buffer gets saved.  This function checks to see if the file being saved is the Emacs.org file you're looking at right now, and if so, automatically exports the configuration here to the associated output files.

    ;; Automatically tangle our Emacs.org config file when we save it
    (defun efs/org-babel-tangle-config ()
      (when (string-equal (buffer-file-name)
    		      (expand-file-name "~/.dotfiles/emacs.org"))
        ;; Dynamic scoping to the rescue
        (let ((org-confirm-babel-evaluate nil))
          (org-babel-tangle))))
    
    (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))


<a id="org6603013"></a>

## Use-package

Use-package is like a package manager. [Use-Package Documentation](https://github.com/jwiegley/use-package)
Package archives: Elpa is default, Melpa is community.

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


<a id="org5947687"></a>

## Automatic Package Updates

The auto-package-update package helps us keep Emacs packages up to date.  It will prompt you after a certain number of days either at startup or at a specific time of day to remind you to update your packages.

You can also use `M-x auto-package-update-now` to update right now!

    (use-package auto-package-update
      :custom
      (auto-package-update-interval 30)
      (auto-package-update-prompt-before-update t)
      (auto-package-update-hide-results t)
      :config
      (auto-package-update-maybe)
      (auto-package-update-at-time "09:00"))


<a id="orgf1410de"></a>

# General Emacs


<a id="orgd730219"></a>

## UI Configurations


### Basic

    (setq inhibit-startup-message t)           ; inhibit startup message
    (tool-bar-mode -1)                         ; remove toolbar
    (menu-bar-mode -1)                         ; Disable the menu bar
    (scroll-bar-mode -1)                       ; remove side scrollbar
    (tooltip-mode -1)                          ; Disable tooltips
    (set-fringe-mode 10)                       ; Give some breathing room
    (setq visible-bell t)                      ; Set up the visible bell
    (save-place-mode 1)                        ; Open file where last visited
    (global-auto-revert-mode 1)                ; refresh buffer if changed on disk
    
    (add-hook 'text-mode-hook 'flyspell-mode)  ; enable spellcheck on text mode
    ;; (add-hook 'prog-mode-hook 'hl-line-mode)   ; highlight lines when programming
    
    ;; Open text files in Org-Mode
    ;; (add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))


### Scrolling

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

1.  Fast Scroll

    To ensure scrolling is fast in Emacs, disable  non-essential things while the window is being scrolled:
    
        (use-package fast-scroll
          :ensure t
          :demand t
          :config
          (add-hook 'fast-scroll-start-hook (lambda () (flycheck-mode -1)))
          (add-hook 'fast-scroll-end-hook (lambda () (flycheck-mode 1)))
          (fast-scroll-config)
          (fast-scroll-mode 1))


### Undo-tree

-   `C-x u` visualizes undo history as a tree for easy navigation
-   `C-_` undo
-   `M-_` redo

    (use-package undo-tree
      :config
      (global-undo-tree-mode 1))


### Backup files

Save backups in `.emacs.d`.

    (setq backup-directory-alist
          '( ("." . "~/.dotfiles/.emacs.d/filebackups")))


### Line numbers

1.  Linum

         (dolist (mode '(org-mode-hook
        		 term-mode-hook
        		 treemacs-mode-hook
        		 eshell-mode-hook
        		 vterm-mode-hook))
           (add-hook mode (lambda () (linum-mode 0))))
        
        (global-linum-mode 1)

2.  Display Line Numbers NOT USED

         (dolist (mode '(org-mode-hook
        		 term-mode-hook
        		 treemacs-mode-hook
        		 eshell-mode-hook
        		 vterm-mode-hook))
           (add-hook mode (lambda () (display-line-numbers-mode 0))))
        
        (global-display-line-numbers-mode t)


### Theme

     (use-package monokai-theme
         :init (load-theme 'monokai t))
    ;;;;;; Saving my SECOND favorite theme which is easier on the eyes.
    ;; (use-package gruvbox-theme
    ;;     :init (load-theme 'gruvbox-dark-hard t))


### Modeline

1.  Doom-modeline

    **NOTE**: The first time you load your configuration on a new machine, you’ll need to run `M-x all-the-icons-install-fonts` so that mode line icons display correctly. (Fixed?)
    
        (use-package all-the-icons
        :init
        (when (and (not (member "all-the-icons" (font-family-list))) ;; autoinstall fonts
        	   (window-system))
          (all-the-icons-install-fonts t)))
        
        (use-package doom-modeline
          :init (doom-modeline-mode 1)
          :custom ((doom-modeline-height 15)))


### Transparency

     (defun transparency (value)
       "Sets the transparency of the frame window. 0=transparent/100=opaque"
       (interactive "nTransparency Value 0 - 100 opaque:")
       (set-frame-parameter (selected-frame) 'alpha value))
    
    (transparency 96)  ;; Default value generally e [94,96]


### Auto-clean white space

    (use-package ws-butler
      :hook ((text-mode . ws-butler-mode)
    	 (prog-mode . ws-butler-mode)))


### Make  `yes or no` prompts shorter

    (defalias 'yes-or-no-p 'y-or-n-p)


### Dashboard

Dash board for initial startup of emacs. [github link](https://github.com/emacs-dashboard/emacs-dashboard)

-   For the icons to display correctly, I needed  to execute `all-of-the-icons-install-fonts`.

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
      (setq dashboard-projects-backend 'project-el)
    
      (dashboard-modify-heading-icons '((recents . "file-text")))
    
      (setq dashboard-set-footer nil)
      )


<a id="orgb91a11c"></a>

## Dired

More to do at [here](https://youtu.be/PMWwM8QJAtU): dired-open

-   "W" will open file in native environment (including another Emacs)
-   "(" toggle file info
-   M-x du  shows the size of the files in the buffer (toggle for human readable)

    (use-package dired
      :ensure nil
      :commands dired
      :config
      (setq dired-listing-switches "-agho --group-directories-first" )
      (setq dired-dwim-target t);; guess other directory for copy and rename
      (define-key dired-mode-map (kbd "C-o") 'other-window))
    
    ;; nice icons in dired
    (use-package treemacs-icons-dired
      :after dired
      :defer t
      :config (treemacs-icons-dired-mode) )
    
    ;; janky mode which lists the recursive size of each foler/item in dired.
    (use-package dired-du
      :commands dired-du-mode
      :defer t)
    
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


<a id="orga80c592"></a>

## Native Compilation


### Suppress compilation warnings

    (setq native-comp-async-report-warnings-errors nil)


<a id="orga0e082c"></a>

## Goto last change

Sometimes it's useful to step to the last change in a buffer.

    (use-package goto-last-change
      :ensure t
      :bind ("C-;" . goto-last-change))
      ;; :hook (org-mode . goto-last-change))


<a id="org2ab0404"></a>

## Input Buffer, Directory Search


### Ivy, Ivy-Rich, and Counsel

Ivy displays vertical completions of input buffer.

    (use-package ivy
      :delight ivy-mode
      :config
      (ivy-mode 1)
      ;; remove ^ on the inputbuffer
      (setq ivy-initial-inputs-alist nil))


### Ivy-Rich

Ivy-rich provides information to display in input buffer to counsel.

    (use-package ivy-rich
      :after ivy
      :init  
      (ivy-rich-mode 1))


### Ivy-prescient

prescient.el provides some helpful behavior for sorting Ivy completion candidates based on how recently or frequently you select them. This can be especially helpful when using M-x to run commands that you don’t have bound to a key but still need to access occasionally.

    (use-package ivy-prescient
      :after counsel
      :custom
      (ivy-prescient-enable-filtering nil)
      :config
      ;; Uncomment the following line to have sorting remembered across sessions!
      (prescient-persist-mode 1)
      (ivy-prescient-mode 1))


### Counsel

Counsel displays ivy-rich info along with suggestions in input buffer.

-   `M-o` allows access to help in input buffer.

    (use-package counsel
      :bind (("M-x" . counsel-M-x)      ; displays ivy-rich info in minibuffer
    	 ("C-x C-f" . counsel-find-file)
    	 :map minibuffer-local-map
    	 ("C-r" . 'counsel-minibuffer-history)
    	 ))


<a id="org8c374b0"></a>

## Helpful and Which-key


### Helpful

Better version of help. We remap normal help keys to Helpful's versions.

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


### Which-key

    (use-package which-key
      :defer 0
      :delight which-key-mode  
      :config(which-key-mode)
      (setq which-key-idle-delay 0.8))


<a id="orgf1fd661"></a>

## TODO Grammarly

There looks to be several packages at the moment. Top two (as of 1/10/22) are installed here without proper hooks. 


### lsp-grammarly

Gives warning on startup for login.   [lsp-grammarly doc](https://github.com/emacs-grammarly/lsp-grammarly)

    (use-package lsp-grammarly
    :ensure t
    :hook (text-mode . (lambda ()
    		     (require 'lsp-grammarly)
    		     (lsp))))


### flycheck-grammarly

Works w/o being logged in.                         [flycheck-grammarly doc](https://github.com/emacs-grammarly/flycheck-grammarly)

    (use-package flycheck-grammarly
    :config
    (setq flycheck-grammarly-check-time 0.8))


<a id="orgc2bcbd0"></a>

## Keybindings

    (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
    (global-set-key (kbd "C-o") 'other-window)
    (global-set-key (kbd "C-x C-b") 'buffer-menu) ;; open buffer menue in current buffer
    (global-set-key (kbd "C-x C-k") 'kill-current-buffer) ;; "C-x k" asks which buffer
    
    ;; Make font bigger/smaller.
    (global-set-key (kbd "C-=") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)
    (global-set-key (kbd "C-0") 'text-scale-adjust)
    
      ;; (global-unset-key (kbd "C-<SPC>"))
      ;; (global-unset-key (kbd "C-m"))
      ;; (global-set-key (kbd "C-m") 'set-mark-command)
      ;; (global-set-key (kbd "C-<SPC>") 'other-window)
      ;; (global-set-key (kbd "M-SPC") 'other-window)

**Future:** create my own keybindings as shown [here](https://www.youtube.com/watch?v=xaZMwNELaJY). hydra ties related commands into short bindings with a common prefix.


<a id="org3903c7e"></a>

## Font size

    (set-face-attribute 'default nil :height 110) ;; needed on laptop


<a id="orgcb3163e"></a>

# General Development


<a id="org8a438b2"></a>

## Flyspell comments

    (add-hook 'prog-mode-hook #'flyspell-prog-mode)


<a id="org66744b4"></a>

## Parens/delimiters

    (show-paren-mode    1) ; Highlight parentheses pairs.


### Rainbow Delimiters

    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))


### Smartparens

Auto-creates closing parenthesis and bar and, smartly, writes it over if it is typed.

    (use-package smartparens
      :delight smartparens-mode
      :hook (prog-mode . rainbow-delimiters-mode))


<a id="orgea12722"></a>

## Magit

[Magit Documentation](https://magit.vc/)

    (use-package magit
      :commands (magit-status)
      :custom
      ;display Magit status buffer in the same buffer rather than splitting it. 
      (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


<a id="orge67c736"></a>

## Git-Gutter

Have it installed to be turned on and off when wanted&#x2013; depends on:

-   linum vs display-line-numbers  (fringe or normal)
-   TTy vs not    [reference](https://github.com/nonsequitur/git-gutter-fringe-plus)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">git-gutter+.el</th>
<th scope="col" class="org-left">git-gutter-fringe+.el</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Works in tty frame</td>
<td class="org-left">+</td>
<td class="org-left">-</td>
</tr>


<tr>
<td class="org-left">Works with linum-mode</td>
<td class="org-left">-</td>
<td class="org-left">+</td>
</tr>


<tr>
<td class="org-left">Gutter on the right side</td>
<td class="org-left">-</td>
<td class="org-left">+</td>
</tr>
</tbody>
</table>


### Git Gutter+

    (use-package git-gutter+
      :after git-gutter)


### Git Gutter-fringe+

    (use-package git-gutter-fringe+
      :commands git-gutter+-mode
      :defer t)


<a id="org340eba9"></a>

## Projectile NOT USED

Allows me to set project-wide commands and variables. [Projectile Documentation](https://docs.projectile.mx/projectile/index.html)
Notably: run, debug, project-variables, grep (and rg).

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


<a id="org5f67c80"></a>

## Company-Mode

Currently company-mode gets called with lsp-mode by default. *my understanding*: company-mode provides the auto-complete box that lsp provides information to.

Issue: company mode not working in org-mode. Correct completion keys are not clear.

    (use-package company
      :ensure t
      :custom
      (company-minimum-prefix-length 1)
      (company-idle-delay 0.5)
      ;; (global-set-key (kbd "C-<tab>") 'company-complete)
    )
    (global-company-mode 1)


### company-box-mode

Brings up a another box with information about the highlighted recommended item in the company/lsp box.

    (use-package company-box
      :delight company-box-mode
      :hook (company-mode . company-box-mode))


### company-prescient

Help in sorting the completion results.

    (use-package company-prescient
      :defer 2
      :after company
      :config
      (company-prescient-mode +1))


<a id="orgb56cbc8"></a>

## LSP and DAP


### lsp-mode

Provides language backend to company-mode.

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
      ;; (setq lsp-headerline-breadcrumb-enable nil)
      ;; (setq lsp-ui-doc-enable nil)
      ;; (setq lsp-completion-enable-additional-text-edit nil)
    
    
      ;; `-background-index' requires clangd v8+!
      (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
      )

The last line concerning `cangd` comes from [mortens.dev](https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/index.html).


### lsp-ui

Provides additional lsp information to the company-mode box. The mode provides info when hoovered by mouse. [lsp-ui documentation](https://emacs-lsp.github.io/lsp-ui/)

**Note:** Functions also display the proceeding C++ function comments as documentation

    (use-package lsp-ui
      :hook (lsp-mode . lsp-ui-mode) ; for elpy
      :custom
      (lsp-ui-doc-position 'bottom))


### lsp-ivy

[lsp-ivy](https://github.com/emacs-lsp/lsp-ivy) integrates Ivy with `lsp-mode` to make it easy to search for things by name in your code.  When you run these commands, a prompt will appear in the minibuffer allowing you to type part of the name of a symbol in your code.  Results will be populated in the minibuffer so that you can find what you're looking for and jump to that location in the code upon selecting the result.

Try these commands with `M-x`:

-   `lsp-ivy-workspace-symbol` - Search for a symbol name in the current project workspace
-   `lsp-ivy-global-workspace-symbol` - Search for a symbol name in all active project workspaces.

    (use-package lsp-ivy
      :after lsp)


### lsp-treemacs

Shows file contents: classes functions etc

-   See lsp-treemacs-references

    (use-package lsp-treemacs
      :after lsp)


### Dap Debugging

Like lsp-mode but for debuggers.

    (use-package dap-mode
      :commands dap-mode)


<a id="orga1b2aa9"></a>

## Flycheck

Checks the code for bugs on the fly.

    (use-package flycheck
      :diminish flycheck-mode
      :after lsp)


<a id="orgd2874fe"></a>

## CMake

Lsp-mode requires the language server on the system:
`pip install cmake-language-server`.


### CMake-mode

    (use-package cmake-mode
      :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
      :hook (cmake-mode . lsp))
    
    (use-package cmake-font-lock
    :ensure t
    :after cmake-mode
    :config (cmake-font-lock-activate))


### CMake project

In the source directory containing `CMakeLists.txt` run `M-x cmake-project-configure-project`.
As a preference, use the `/bin/` option to keep the cmake files out of the source directory.
After this, the `compile` automatically holds the correct command.

    (use-package cmake-project
      :hook ((c++-mode . cmake-project-mode )
    	 (c-mode . cmake-project-mode))
      )


<a id="orgccdfa5e"></a>

## Yasnippet

    (use-package yasnippet
      :delight( yas-minor-mode)
      :after lsp)
    
    (use-package yasnippet-snippets
      :after yas-minor-mode) ; load basic snippets from melpa
    
    (yas-global-mode 1)


<a id="orgd0c0514"></a>

## Flyspell

    (use-package flyspell
      :ensure nil
      :bind (:map flyspell-mode-map ("C-;" . nil)))


<a id="orge7ec481"></a>

## TODO Evil nerd commenter

    (use-package evil-nerd-commenter
    :bind ("M-;". evilnc-comment-or-uncomment-lines))


<a id="org36c6c36"></a>

# Emacs-lisp

    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)


<a id="org17aacc8"></a>

# Bash

-   Flycheck uses shellcheck, which requires `sudo dnf install -y shellcheck`
-   debuggers are available
-   If language server doesn't install automatically either:
    1.  `sudo dnf -y nodejs-bash-language-server.noarch`
    2.  `M-x lsp-install-server`

    (defun my-sh-mode-hook-fn()
      (setq sh-basic-offset 2
    	sh-indentation 2) ;; defaults are 4
      (lsp))
    
    
    (add-hook 'sh-mode-hook #'my-sh-mode-hook-fn)


<a id="orge6ad938"></a>

# Python


<a id="orgf439601"></a>

## TODO Jupyter Notebooks


### TODO run Jupyter in babel

[Reference](https://sqrtminusone.xyz/posts/2021-05-01-org-python/) and <https://github.com/jkitchin/scimax>
Cannot get zmq (Jupyter dependency)  to work currently

    (use-package jupyter
        :after (org)
        ;; :straight t
        )
    
    (org-babel-do-load-languages 'org-babel-load-languages
    			     (append org-babel-load-languages
    				     '((jupyter . t))))


### TODO open notebook in Emacs

If I use EIN, add the setting for displaying the figures inline.

    (use-package ein
      :commands (ein:notebooklist-open)
      ;; :config
      ;; (require 'ein-loaddefs)
      ;; (require 'ein)
      ;; (define-key ein:notebook-mode-map (kbd "<C-tab>") 'my-function)
      )


### TODO completion

In [EIN video](https://www.youtube.com/watch?v=OB9vFu9Za8w), Miller says that the completion is based on `auto-complete` rather than `company`. So here we are going to try to get completion setup for `EIN`.
Also, in [EIN github](https://github.com/millejoh/emacs-ipython-notebook#ob-ein), Miller says that `EIN` is an `elpy` module, so maybe we need `elpy` for completion?


<a id="orga731adc"></a>

## Pyvenv

    (use-package pyvenv
    :ensure t
    :defer t
    :diminish
    :config
    
    (setenv "WORKON_HOME" "/home/ape/.conda/envs")
    	; Show python venv name in modeline
    	(setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
    	(pyvenv-mode t))

After package installation, you should have `M-x pyvenv-workon` command with a list of your virtual environments.

The only lack of this is that you need to restart LSP workspace at least once when you change venv by pyvenv-workon command.

So the flow should be like this:

`M-x pyvenv-workon <your-venv>`
`M-x lsp-restart-workspace`

After changing venv all installed packages from venv should be visible for LSP server.


<a id="org55a796e"></a>

## Python-mode


### Pyright

    ; npm must be installed on the system.
      (use-package lsp-pyright
        :after lsp
        :hook (python-mode . (lambda ()
    			    (require 'lsp-pyright)
    			    (lsp))))


### python-mode

    (use-package python-mode
      :ensure nil ; don't install, use the pre-installed version
    
      :custom
      (python-shell-completion-native-enable 1)
      (python-shell-interpreter "ipython")
      (python-shell-interpreter-args "-i --simple-prompt")
      ; this command doesn't work BUT without, python-mode "won't load".
      :bind (:map python-mode-map ("C-RET" . python-shell-send-statement))
      )


<a id="orgae1182a"></a>

## Hook

    (defun my-python-mode-hook-fn ()
      (lsp)
      (require 'dap-python)
      ;; (local-set-key (kbd "<tab>") #'company-indent-or-complete-common)
      )
    
    (add-hook 'python-mode-hook #'my-python-mode-hook-fn)


<a id="org18c8d06"></a>

# C/C++


<a id="orgfd662e7"></a>

## Compilation Buffer

Have the `*Compilation*` buffer scroll with the output.

    (setq compilation-scroll-output t)

The following keeps the compilation buffer if there are warnings or errors, and buries it otherwise (after 1 second). [source](https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close)

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


<a id="org9ad03d2"></a>

## Hook

Currently lsp-mode works with clangd backend without any initial setup.
company-clang needs `clang` installed on the system.

    (setq-default c-basic-offset 2)
    
    (defun my-c-c++-mode-hook-fn ()
      (lsp)                ; turn on
      (local-set-key (kbd "C-<tab>") #'lsp-format-buffer) ;tab comp
      (smartparens-mode 1)
      )
    
    (add-hook 'c-mode-hook #'my-c-c++-mode-hook-fn)
    (add-hook 'c++-mode-hook #'my-c-c++-mode-hook-fn)


<a id="org5a2490c"></a>

# Org-Mode


<a id="org7f1b758"></a>

## Mode setup

    (defun jmn/org-mode-setup ()
      (org-indent-mode)
      (variable-pitch-mode 1)
      (visual-line-mode 1)
      (rainbow-delimiters-mode 0)
      )


<a id="org30aae49"></a>

## Fonts

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


<a id="org0f124e3"></a>

## Start

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


<a id="org6ea41c2"></a>

## Bullets

    (use-package org-bullets
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


<a id="orga2e023d"></a>

## Center column

    (defun efs/org-mode-visual-fill ()
      (setq visual-fill-column-width 100
    	visual-fill-column-center-text t)
      (visual-fill-column-mode 1))
    
    (use-package visual-fill-column
      :hook (org-mode . efs/org-mode-visual-fill))


<a id="org2598b9c"></a>

## Org-babel

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


<a id="org60d2a63"></a>

## Inline latex

Note: I had to install texlive dependencies for latex framents to work. I found what needed to be installed by running `pdflatex` on the generated tex file in `/tmp/` created by org.

**Font size**:

    (defconst jmn-latex-scale 1.0 "scaling factor for latex fragments")
    (setq org-format-latex-options (plist-put org-format-latex-options :scale jmn-latex-scale))

Create a function to align the size of displayed latex framents with overall org-mode font size.

    (defun update-org-latex-fragments ()
      (org-latex-preview '(64))
      (plist-put org-format-latex-options :scale (+ jmn-latex-scale  (* 0.3 text-scale-mode-amount)))
      (org-latex-preview '(16)))
    (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)


<a id="org584d8ac"></a>

## Keybindings

    (global-set-key (kbd "C-c l") #'org-store-link)
    (global-set-key (kbd "C-c a") #'org-agenda)
    (global-set-key (kbd "C-c c") #'org-capture)


<a id="org048bb21"></a>

# Terminals


<a id="org9072970"></a>

## vterm

Faster terminal due to being compiled. Default is a better mode than term-mode; it's like a Char-mode but with ability to access function list with M-x.  [vterm Documentation](https://github.com/akermu/emacs-libvterm)

-   For more than one terminal, you must M-x rename-uniquely the terminal.
-   C-c prefix for term commands
-   C-c C-c = send C-c to the terminal (kill running command)

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


<a id="orgef6cfd8"></a>

## term-mode

-   Slower than vterm at printing large amounts of information.
-   For more than one terminal, you must M-x rename-uniquely the terminal.
-   C-c prefix for term commands

Line-mode vs char-mode **selection shows on the modeline**:
C-c C-k -> char-mode
C-c C-j  -> line-mode


### Better term-mode colors

    (use-package eterm-256color
      :hook (term-mode . eterm-256color-mode))


<a id="org4c43bf5"></a>

## shell-mode

Between term-mode and eshell.


<a id="org9ec6015"></a>

## eshell

More customization in Elisp. Supports Tramp. 


<a id="orga70b75f"></a>

# Localwords


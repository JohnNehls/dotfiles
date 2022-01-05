;; Install some packages
(defvar myPackages
  '(better-defaults ; better theme
    zeno-theme
    neotree ; sidebar directory tree plugin
    magit ; Git porcelain
    ;;;; pythonIDE packages
    elpy ; wraps jedi, flymake, yasnippit all in one package
    company-quickhelp ; used to provide helpbox with auto-complete suggestion's help
    ;;;; c/cpp IDE packages
    lsp-mode           ; Language server protocol for interpreting languages 
    yasnippet          ; Code templates
    lsp-treemacs       ; Tree renderer-- have not explored yet
    ;helm-lsp           ; Helm is for incremental completion/selection
    projectile         ; Project interaction library
    hydra    ; ties related commands into short bindings with a common prefix
    flycheck ; On-the-fly syntax checking, replacemnt for flymake
    company  ; Text completion
    avy      ; Jumping to visible text using a char-based decision tree
    which-key ; Displays key bindings following entered incomplete command
    ;helm-xref ; Helm interface for xref
    dap-mode  ; Debug Adapter Protocol is a wire protocol for communication between client and Debug Server
    )
  )
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; the following lines enable the elpy package as well as the ipython shell
(elpy-enable)
(setq python-shell-interpreter "ipython" ;require install ipython
      python-shell-interpreter-args "-i --simple-prompt")
(add-hook 'python-mode-hook 'eldoc-mode)
(setq elpy-rpc-python-command "python3")
(setq elpy-shell-echo-output nil)
(setq python-shell-completion-native-enable nil)
(setq python-indent-offset 4
      python-indent 4)
;;;;;; (setq elpy-rpc-backend "jedi") ;;; not needed as of now
 
;;;;;; quick help box in autocompete
(company-quickhelp-mode 1)
(eval-after-load 'company '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)) 
(setq company-quickhelp-color-background "dim gray")
;;;;;;; END Python IDE configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

;; setting to allow sizing of JPG and PNGs in org-mode
(setq org-image-actual-width nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

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
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package solarized-theme
  :init (load-theme 'solarized-gruvbox-dark t))

(load-theme 'solarized-gruvbox-dark t)

(require 'color)
(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(transparency 96)

(setq inhibit-startup-message t)          ; inhibit startup message
(tool-bar-mode -1)                        ; remove toolbar
(scroll-bar-mode -1)                      ; remove side scrollbar
(tooltip-mode -1)                     ; Disable tooltips
;(menu-bar-mode -1)                   ; Disable the menu bar
(set-fringe-mode 10)                  ; Give some breathing room
;(global-display-line-numbers-mode t) ; show line numbers (better)
(global-visual-line-mode t)       ; removes coninuation arrow
(setq make-backup-files nil)      ; stop creating backup~ files
(setq auto-save-default nil)      ; stop creating #autosave# files
(add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode)) ; open texts in org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Set up the visible bell
(setq visible-bell t)

;; Set up the visible bell
(setq visible-bell t)

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(add-hook 'text-mode-hook 'flyspell-mode)    ; enable spellcheck on text mode

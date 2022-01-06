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

(use-package command-log-mode)

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq inhibit-startup-message t)           ; inhibit startup message
(tool-bar-mode -1)                         ; remove toolbar
(scroll-bar-mode -1)                       ; remove side scrollbar
(tooltip-mode -1)                          ; Disable tooltips
(set-fringe-mode 10)                       ; Give some breathing room
(global-visual-line-mode t)                ; removes coninuation arrow
(add-hook 'text-mode-hook 'flyspell-mode)  ; enable spellcheck on text mode
(setq make-backup-files nil)               ; stop creating backup~ files
(setq auto-save-default nil)               ; stop creating #autosave# files
;(menu-bar-mode -1)                        ; Disable the menu bar


;; Open text files in Org-Mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode)) 
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Set up the visible bell
(setq visible-bell t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ;Disable line numbers for some modes
;;  (dolist (mode '(org-mode-hook
;; 		 term-mode-hook
;; 		 shell-mode-hook
;; 		 eshell-mode-hook))
;;    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (global-display-line-numbers-mode t)     ; Puts line numbers on ALL buffers

(use-package gruvbox-theme
    :init (load-theme 'gruvbox t))

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(transparency 96)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ;("C-r" . 'counsel-minibuffer-history)
	 ))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode  ;remove name from minor mode list
  :config
  (setq which-key-idle-delay 1.0))

(use-package neotree)

(use-package magit)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package elpy
  :init (elpy-enable) ;enables Elpy in all future python buffers
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (elpy-rpc-python-command "python3")
  (elpy-shell-echo-output nil)
  ;(elpy-rpc-backend "jedi")
  ;; Not sure if the following should really be here
  (python-shell-completion-native-enable nil)
  (python-indent-offset 4)
  (python-indent 4)
  )

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

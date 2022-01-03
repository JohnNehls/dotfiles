;; Commented out to increase startup time
;; adds the Melpa archive to the list of package repositories and then gives permission to Emacs to use these packages.
;(require 'package)
;(add-to-list 'package-archives
;            '("melpa" . "http://melpa.org/packages/") t)
;(package-initialize)
;(package-refresh-contents)

;;;; TEST EXPERIMENTAL COMMIT

;; Install some packages
(defvar myPackages
  '(better-defaults ; better theme
    material-theme
    neotree ; sidebar directory tree plugin
    magit ; Git porcelain
    ;;;; pythonIDE packages
    elpy ; wraps jedi, flymake, yasnippit all in one package
    company-quickhelp ; used to provide helpbox with auto-complete suggestion's help
    ;;;; c/cpp IDE packages
    lsp-mode           ; Language server protocol for interpreting languages 
    yasnippet          ; Code templates
    lsp-treemacs       ; Tree renderer-- have not explored yet
    helm-lsp           ; Helm is for incremental completion/selection
    projectile         ; Project interaction library
    hydra    ; ties related commands into short bindings with a common prefix
    flycheck ; On-the-fly syntax checking, replacemnt for flymake
    company  ; Text completion
    avy      ; Jumping to visible text using a char-based decision tree
    which-key ; Displays key bindings following entered incomplete command
    helm-xref ; Helm interface for xref
    dap-mode  ; Debug Adapter Protocol is a wire protocol for communication between client and Debug Server
    )
  )
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;;;;; Global Configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)          ; inhibit startup message
(tool-bar-mode -1)                        ; remove toolbar
(scroll-bar-mode -1)                      ; remove side scrollbar
(load-theme 'material t)          ; ??
(global-display-line-numbers-mode t) ; show line numbers (better)
(global-visual-line-mode t)       ; removes coninuation arrow
(setq make-backup-files nil)      ; stop creating backup~ files
(setq auto-save-default nil)      ; stop creating #autosave# files
(add-to-list 'auto-mode-alist '("\\.text\\'" . org-mode)) ; open texts in org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;;;;;;;;; Set transparency of emacs
 (defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(transparency 96)

;;;;;;;;;; NeoTree ;;;;;;;;;;;;;;;;;;


;;;;;;;;; Org mode configs ;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;; Text mode config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook 'flyspell-mode)    ; enable spellcheck on text mode

;;;;;;;; BEGIN Python IDE configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; BEGIN CPP IDE  https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/ ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

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
;;;;;;;;;;;;;;;;;; END C IDE sample configuration file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes '(tsdh-dark))
 '(elpy-autodoc-delay 0.1)
 '(fci-rule-color "#37474f")
 '(hl-sexp-background-color "#1c1f26")
 '(package-selected-packages
   '(magit which-key projectile material-theme jedi-core irony-eldoc helm-xref helm-lsp flycheck elpy dap-mode company-quickhelp better-defaults auto-complete))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a")))
 '(vc-annotate-very-old-color nil))


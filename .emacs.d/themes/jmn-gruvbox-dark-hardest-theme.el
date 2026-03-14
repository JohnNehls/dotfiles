(deftheme jmn-gruvbox-dark-hardest
  "Altering the gruvbox-dark-hard theme-- load after loading the gruvbox theme")

(let ((done-color "gray35"))

  ;; changes which will be permanent  
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          `(("NEXT" . ,(face-foreground font-lock-function-name-face))
            ("HOLD" . ,(face-foreground font-lock-builtin-face))
            ("DONE" . done-color)
            ("IGNORE" . done-color)
            )))

  ;; changes which will be undone when disabling the theme
   (custom-theme-set-faces
    'jmn-gruvbox-dark-hardest
    ;; Basics
    `(default ((t (:background "gray7" :extend t))))
    `(link ((t (:foreground "#83a598" :extend t))))  ;;'tree-sitter-hl-face inherits it

    `(line-number ((t :background "gray7" :extend t )))
    `(fringe ((t :background "gray7" :extend t )))

    ;; treesitter
    `(tree-sitter-hl-face:variable ((t :foreground "#FFA500" :extend t)))
    `(tree-sitter-hl-face:variable.parameter ((t :foreground ,(face-attribute 'default :foreground))))
    `(tree-sitter-hl-face:label ((t :foreground "#d65d0e" :extend t)))
    
    ;; org    
    `(org-agenda-done ((t (:foreground ,done-color))))
    `(org-headline-done ((t (:foreground ,done-color))))
    `(org-done ((t (:foreground ,done-color))))
    `(org-block ((t (:background "gray3" :extend t))))
    )
)

(provide-theme 'jmn-gruvbox-dark-hardest)

(deftheme jmn-gruvbox-dark-hard
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
    'jmn-gruvbox-dark-hard
    ;; Basics

    `(mode-line-inactive ((t (:background "gray22" :extend t))))
    `(mode-line-active ((t (:background "gray35" :extend t))))
    
    `(line-number ((t :background ,(face-attribute 'default :background))))
    `(fringe ((t :background ,(face-attribute 'default :background))))

    ;; org    
    `(org-agenda-done ((t (:foreground ,done-color))))
    `(org-headline-done ((t (:foreground ,done-color))))
    `(org-done ((t (:foreground ,done-color))))
    `(org-block ((t (:background "gray8" :extend t))))
    )
)

(provide-theme 'jmn-gruvbox-dark-hard)

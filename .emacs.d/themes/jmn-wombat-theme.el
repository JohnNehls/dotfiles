(deftheme jmn-wombat
  "Altering the default theme")

;; variables which will be permanent
(with-eval-after-load 'org
  (setq org-todo-keyword-faces
            `(("TODO" . "Pink")
              ("NEXT" . "gold2")
              ("HOLD" . "orange3")
              ("IGNORE" . "#99968b"))))


  (custom-theme-set-faces
   'jmn-wombat

   ;; Basics
   `(mode-line ((t (:background "gray44" :extend t))))
   
   ;; Org-mode
   `(org-block ((t (:background "gray10" :extend t))))
   `(org-level-7 ((t (:background "MediumPurple1" :extend t))))

   ;; Tab bar
   '(tab-bar ((t (:inherit org-default :extend t))))
   '(tab-bar-tab ((t (:inherit mode-line :weight bold :box nil :extend t))))
   '(tab-bar-tab-inactive ((t (:inherit org-default :extend t))))
   )


(provide-theme 'jmn-wombat)

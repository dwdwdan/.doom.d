;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(cond ((eq system-type 'windows-nt)
       (setq sync-dir "C:/Users/Daniel Walters/Dropbox")
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'gnu/linux)
       (setq sync-dir "~/Nextcloud")
       ;; Linux-specific code goes here.
       ))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Daniel Walters"
      user-mail-address "dan.walters5@outlook.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
(setq doom-font (font-spec :family "Jetbrains Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "Arial" :size 20))

(setq doom-theme 'doom-dracula)

(setq org-directory (concat sync-dir "/Org")
      org-roam-directory (concat sync-dir "/OrgRoam")
      org-ellipsis " ▼"
      org-superstar-headline-bullets-list `("◉" "○")
      org-agenda-span 7
      org-agenda-start-on-weekday 1
      org-agenda-start-day "+0d"
      org-log-into-drawer t
      org-startup-with-latex-preview t)

(after! org
        (dolist (face `((org-level-1 . 1.5)
                        (org-level-2 . 1.4)
                        (org-level-3 . 1.3)
                        (org-level-4 . 1.2)
                        (org-level-5 . 1.1)
                        (org-level-6 . 1.1)
                        (org-level-7 . 1.1)
                        (org-level-8 . 1.05)))
        (set-face-attribute (car face) nil :weight `bold :height (cdr face)))
        (set-face-attribute `org-document-title nil :height 300)
        (set-face-attribute `org-block nil :foreground nil :background "#353848" :inherit `fixed-pitch)
        (set-face-attribute `org-code nil :inherit `(shadow fixed-pitch))
        (set-face-attribute `org-table nil :background "#353848" :inherit `(shadow fixed-pitch))
        (set-face-attribute `org-verbatim nil :inherit `(shadow fixed-pitch))
        (set-face-attribute `org-special-keyword nil :inherit `(font-lock-comment-face fixed-pitch))
        (set-face-attribute `org-meta-line nil :inherit `(font-lock-comment-face fixed-pitch))
        (set-face-attribute `org-checkbox nil :inherit `fixed-pitch)

        (setq org-todo-keywords `((sequence "TODO(t)" "IN PROGRESS(p)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)")))
        (setq org-refile-targets `((,(concat org-directory "/archive.org") :maxlevel . 2)
                                   (,(concat org-directory "/todo.org") :maxlevel . 1)))
        (setq org-capture-templates `(("t" "Todo" entry (file ,(concat org-directory "/inbox.org")) "* TODO %?\n %U\n %a\n %i" :empty-lines 1))))

(setq +latex-viewers `(pdf-tools))

(setq display-line-numbers-type `relative)

(defun dan/org-setup ()
  (variable-pitch-mode 1)
  (org-indent-mode)
  (display-line-numbers-mode 0)
  (org-fragtog-mode))

(add-hook! org-mode (dan/org-setup))

(use-package! visual-fill-column
  :hook (org-mode . dan/org-visual-fill))

(defun dan/org-visual-fill ()
  (setq visual-fill-column-width 125)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(advice-add `org-refile :after `org-save-all-org-buffers)

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "evolution")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

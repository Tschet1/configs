;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
(fset 'python_debug
   "from pprint import pprint; import pdb; pdb.set_trace()")
(use-package! groovy-mode)
(use-package! string-inflection)
(use-package! jinja2-mode)

(map! :leader
      (:prefix-map ("a" . "mycommands")
        :desc "Next window" "a" #'next-multiframe-window))

(defun my-turn-current-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; Clang stuff
(use-package! clang-format)
(setq clang-format-style "file")

;; Building
(defun test_dragoon (family))

;; JIRA integration
(defun jira-my-issues ()
  "List my open JIRA issues"
  (interactive)
  (shell-command
   "python3 -m jira my-issues"
   ))

(defun jira-lookup-issue-at-point ()
  "Lookup JIRA issue"
  (interactive)
  (let (bounds pos1 pos2 thing issue)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (- (car bounds) 10))
    (setq pos2 (+ (cdr bounds) 10))
    (setq thing (buffer-substring-no-properties pos1 pos2))

    (setq issue
          (shell-command-to-string
           (concat "echo '" thing "' | grep -o '[A-Z][A-Z]*-[0-9][0-9]*'")
           )
          )

    (shell-command
     (concat "python3 -m jira issue " issue)
     )

    )
  )

(map! :leader
      (:prefix-map ("j" . "jira")
       :desc "my issues" "m" #'jira-my-issues
       :desc "lookup issue at point" "j" #'jira-lookup-issue-at-point
       ))

;; create cmocka testcases
(defun cmocka-testcases-to-testsuite ()
  ;; create test-suite entry from test-cases
  (interactive)

  (insert
   (concat "cmocka_unit_test("
           (shell-command-to-string
          (concat "echo " (buffer-string) " | grep -o 'test_[^\(]*'")
            )
           "),"
           )
   )
  )

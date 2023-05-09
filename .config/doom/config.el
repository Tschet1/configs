;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jan Müller"
      user-mail-address "jan.m.muller@me.com")

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

(use-package! graphviz-dot-mode)
(add-hook 'graphviz-dot-mode-hook 'company-mode)

(map! :leader
      (:prefix-map ("a" . "mycommands")
        :desc "Next window" "a" #'next-multiframe-window))

(defun my-turn-current-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; don't add home to projectile
(after! projectile (setq projectile-project-root-files-bottom-up (remove
            ".git" projectile-project-root-files-bottom-up)))

;; define the shell to use
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

;; Clang stuff
(use-package! clang-format)
(setq clang-format-style "file")

;; Building
(defun test_dragoon (family))

;; West
(defun west-update ()
  "Run west update"
  (interactive)
  (shell-command
   "west update"
   ))

;; Clang format
(defun git-clang-format ()
  "Run clang format on recent changes"
  (interactive)
  (shell-command
   "git clang-format"
   ))

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

;;(map! :leader
;;      (:prefix-map ("a" . "applications")
;;       (:prefix ("j" . "journal")
;;        :desc "New journal entry" "j" #'org-journal-new-entry
;;        :desc "Search journal entry" "s" #'org-journal-search)))

;; add to ~/.doom.d/config.el
(after! lsp-ui-mode
  (setq lsp-ui-doc-enable t))

;; set path
(setq exec-path (append exec-path '("/usr/local/sbin" "/Users/jan/.cargo/bin" "/usr/local/bin" "/usr/local/opt/llvm/bin/")))

(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))
(use-package! cmake-build)

(use-package! ripgrep)

(use-package! groovy-mode)
(evil-commentary-mode)
; # Vue
;(use-package! lsp-mode
;  :commands lsp)

;; for completions
;(use-package! company-lsp
;  :after lsp-mode
;  :config (push 'company-lsp company-backends))
;
;(use-package! vue-mode
;  :mode "\\.vue\\'"
;  :config
;  (add-hook 'vue-mode-hook #'lsp))

;; Allow jumping to end of line
(map! :after evil
      "C-e" #'end-of-line)

(use-package! groovy-mode)

(fset 'python_debug
    "from pprint import pprint; import pdb; pdb.set_trace()")

; https://github.com/gregsexton/ob-ipython
(use-package! ob-ipython)
(use-package! lsp-pyright)

;; Templates
(require 'tempo)
(setq tempo-interactive t)
(tempo-define-template "cmocka-test"
                       '("static void test_" (p "Name: " name) "(void **state)" n>
                         "{" n>
                         n>
                         "}")
                       "Add cmocka testcase")

(defun cmocka-testcase-from-line (line)
  "create a cmocka testcase invocation line from the line containing the testcase name"
  (string-match "static\svoid\s\\(test_[^(]*\\)(" line)
  (concat "cmocka_unit_test(" (match-string 1 line) "),")
  )

(tempo-define-template "cmocka-main"
                       '("int main(void)" n>
                         "{" n>
                         "const struct CMUnitTest " (p "Name: " name) "_test_suite[] = {" n>
                         (string-join
                          (sort
                           (seq-map #'cmocka-testcase-from-line
                                    (seq-filter (lambda (elt) (string-match "static\svoid\stest_" elt))
                                                (split-string (buffer-string) "\n")
                                                ))
                           'string-lessp ) "\n"
                          ) n>
                         "};" n>
                         n>
                         "int fail_count = 0;" n>
                         "fail_count += cmocka_run_group_tests(" (s name) "_test_suite, NULL, NULL);" n>
                         n>
                         "return fail_count;" n>
                         "}")
                       "Add cmocka main"
                       )

;; JIRA
(defun current-jira-issue-from-git-branch ()
  (let ((branch (car (vc-git-branches)))
        (regexp "[A-Z]\\{3,\\}-[0-9]\\{4,\\}"))
    (when (string-match regexp branch)
      (message "ok")
        (match-string 0 branch)
      )
    )
  )

(defun enter-current-jira-issue ()
  (interactive)
  (insert (current-jira-issue-from-git-branch))
  )

;; test framework

(defun get-test-suite (buffer)
  (string-match "class\s\\([a-zA-Z]*Test\\)" buffer)
  (match-string 1 buffer)
  )

(defun ctf-build-test (dev testSuite)
  (projectile-run-async-shell-command-in-root (concat "export PATH=$PATH:/home/jam/.docker/cli-plugins; cd test_ble && ./indocker.sh ./build.py -p tests/ -e 'dut.dev.attr_shortname=" dev " ts=*" testSuite "*' "))
  )

(defun ctf-run-test-suite (dev testSuite)
  (projectile-run-async-shell-command-in-root (concat "export PATH=$PATH:/home/jam/.docker/cli-plugins; cd test_ble && ./indocker.sh -p ctf_tm -c session -p tests/ -e 'dut.dev.attr_shortname=" dev " ts=*" testSuite "*' "))
  )

(defun ctf-run-test-case (dev testSuite testCase)
  (projectile-run-async-shell-command-in-root (concat "export PATH=$PATH:/home/jam/.docker/cli-plugins; cd test_ble && ./indocker.sh -p ctf_tm -c session -p tests/ -e 'dut.dev.attr_shortname=" dev " ts=*" testSuite "* tc=*" testCase "*' "))
  )

(defun ctf-build-test-52840 ()
  (interactive)
  (ctf-build-test "52840*" (get-test-suite (buffer-string)))
  )

(defun ctf-run-testsuite-52840 ()
  (interactive)
  (ctf-run-test-suite "52840*" (get-test-suite (buffer-string)))
  )

(defun ctf-build-test-52833 ()
  (interactive)
  (ctf-build-test "52833*" (get-test-suite (buffer-string)))
  )

(defun ctf-run-testsuite-52833 ()
  (interactive)
  (ctf-run-test-suite "52833*" (get-test-suite (buffer-string)))
  )

(defun ctf-build-test-5340 ()
  (interactive)
  (ctf-build-test "5340*" (get-test-suite (buffer-string)))
  )

(defun ctf-run-testsuite-5340 ()
  (interactive)
  (ctf-run-test-suite "5340*" (get-test-suite (buffer-string)))
  )

(map! :leader
      (:prefix-map ("j" . "jira")
       :desc "my issues" "m" #'jira-my-issues
       :desc "lookup issue at point" "j" #'jira-lookup-issue-at-point
       :desc "Enter current jira issue" "e" #'enter-current-jira-issue
       )
      (:prefix-map ("w" . "west")
       :desc "west update" "u" #'west-update
       )
      (:prefix-map ("m" . "my-commands")
       :desc "clang-format" "f" #'git-clang-format
       )
      (:prefix-map ("c" . "ctf")
       (:desc "comment line" "c" #'comment-line)
       (:prefix-map ("b" . "build")
        :desc "52840" "1" #'ctf-build-test-52840
        :desc "52833" "2" #'ctf-build-test-52833
        :desc "5340" "3" #'ctf-build-test-5340
        )
       (:prefix-map ("t" . "test")
        :desc "52840" "1" #'ctf-run-testsuite-52840
        :desc "52833" "2" #'ctf-run-testsuite-52833
        :desc "5340" "3" #'ctf-run-testsuite-5340
        )
       )
      )

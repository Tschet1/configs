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

(use-package! groovy-mode)

(fset 'python_debug
    "from pprint import pprint; import pdb; pdb.set_trace()")

(use-package! lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(setq lsp-python-ms-python-executable-cmd "/usr/local/bin/python3")
(setq lsp-python-ms-executable "/Users/jan/.python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer")

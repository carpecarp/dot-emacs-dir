(message "starting ~/.emacs.d/init.el")
(message (version))
(setq ring-bell-function 'ignore)


; from https://jamiecollinson.com/blog/my-emacs-config/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (git-gutter exec-path-from-shell use-package s )))
 '(safe-local-variable-values (quote ((flycheck-clang-language-standard . c++11))))
 '(show-paren-mode t))


;; So I can evaluate long lists like load-path
;; in *scratch*
(setq eval-expression-print-length 1600)

(unless (boundp 'user-emacs-directory)
  (setq user-emacs-directory "~/.emacs.d"))

;; Use my-lisp directory as a place to put random
;; emacs code that I want to use
(setq load-path
      (let ((load-path (copy-sequence load-path))
	    (local-first (list (concat user-emacs-directory
				       (convert-standard-filename "my-lisp/"))))
	    )
	(append local-first load-path)
	)
      )

;; String manipulation from https://github.com/magnars/s.el
;; "The Long Lost Emacs String Manipulation Library"
;; note: saw how to use package-installed-p in
;; https://github.com/bbatsov/emacs.d/blob/master/init.el
;; bbatsov has a great emacs.el style and emacs knowledge
;; in general
(unless (package-installed-p 's)
  (package-install 's))
(require 's)

;; Stop messing with packages as with package s.el above
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; from https://jamiecollinson.com/blog/my-emacs-config/
;; sets exec-path to be consistent with my $PATH
;; also sets PATH correctly for shells starting in emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; This adds additional extensions which indicate files normally
;; handled by cc-mode.
(setq auto-mode-alist
      (append '(("\\.cc$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.c$"  . c-mode)
                ("\\.h$"  . c-mode))
              auto-mode-alist))

;; Tell cc-mode not to check for old-style (K&R) function declarations.
;; This speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; If we are starting emacs from the toolbar, then our environment
;; is probably not set correctly, or exactly as we want, in particular
;; PATH is not set correctly because .profile was not run
;;; todo

;; modern emacs package loading
;; https://github.com/jwiegley/use-package
;; http://cachestocaches.com/2015/8/getting-started-use-package/
;;
;; More greate emacs init ideas:
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;;
;; great organization into different files
;; https://github.com/gjstein/emacs.d/tree/master/config


;; See https://melpa.org/#/getting-started
;; this is a collection of emacs packages
;; it contains some cool stuff worth
;; exploring later, like using google maps
;; from emacs

(defconst bcarp-c-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (indent-tabs-mode . nil)
    (c-hanging-braces-alist
     (substatement-open before after))
    (c-offsets-alist
     (topmost-intro . 0)
     (substatement . +)
     (substatement-open . 0)
     (case-label . +)
     (access-label . -)
     (inclass . +)
     (inline-open . 0)))
  "Bill Carp C++ Programming Style")

(defconst bnr-c-style
  '((c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (indent-tabs-mode . nil)
    (c-hanging-braces-alist
     (substatement-open before after))
    (c-offsets-alist
     (topmost-intro . 0)
     (substatement . +)
     (substatement-open . 0)
     (case-label . +)
     (access-label . -)
     (inclass . +)
     (inline-open . 0)))
  "BossaNova C++ Programming Style")

(defconst orcadt-c-style
  '((c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (indent-tabs-mode . nil)
    (c-hanging-braces-alist
     (substatement-open before after))
    (c-offsets-alist
     (topmost-intro . 0)
     (substatement . +)
     (substatement-open . 0)
     (case-label . +)
     (access-label . -)
     (inclass . +)
     (inline-open . 0)))
  "OrcaDT(JC) C++ Programming Style")

;; (defun my-c-mode-hook ()
;;  (setq c-basic-offset 4
;;	indent-tabs-mode nil
;;	c-indent-level 4
;;	c-default-style "bsd")
;  (c-toggle-hungry-state 1)
;;  )

;; Problem with gitgutter and display-line-numbers-mode
;; interaction: ~/orcadt-bill/dc_11_11_final/dc/rd/
(defun my-c-mode-hook ()
  (c-add-style "bcarp" bcarp-c-style t)
  (c-add-style "bnrobo" bnr-c-style nil)
  (c-add-style "orcadt" orcadt-c-style nil)
  (c-set-style "bcarp")
  (c-toggle-hungry-state 1)
  (display-line-numbers-mode)
  (git-gutter-mode)
  )
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;; (setq server-use-tcp t)
(server-start)

(global-set-key (kbd "C-x <up> ") 'windmove-up)
(global-set-key (kbd "C-x <down> ") 'windmove-down)
(global-set-key (kbd "C-x <left> ") 'windmove-left)
(global-set-key (kbd "C-x <right> ") 'windmove-right)

;; flycheck setup
(with-eval-after-load 'flycheck
   (require 'flycheck-clang-analyzer)
   (flycheck-clang-analyzer-setup))

;; ispell
(setq ispell-program-name "aspell")

;; git support from https://jamiecollinson.com/blog/my-emacs-config/
;;;(use-package magit
;;;    :ensure t
;;;    :bind ("C-x g" . magit-status))

 (use-package git-gutter
    :ensure t
    :config
    :diminish git-gutter-mode)

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "end ~/.emacs.d/init.el")


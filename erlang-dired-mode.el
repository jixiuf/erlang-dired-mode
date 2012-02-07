;;; erlang-dired.el --- erlang dired mode    -*- coding:utf-8 -*-

;; Description: erlang dired mode
;; Created: 2011-12-20 22:41
;; Last Updated: Joseph 2012-02-07 14:12:27 星期二
;; Author: Joseph(纪秀峰)  jixiuf@gmail.com
;; Maintainer:  Joseph(纪秀峰)  jixiuf@gmail.com
;; Keywords: erlang dired Emakefile
;; URL: http://www.emacswiki.org/emacs/erlang-dired-mode.el
;; X-URL:git://github.com/jixiuf/erlang-dired-mode.git

;; Copyright (C) 2011, 纪秀峰, all rights reserved.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `erlang-emake'
;;    run make:all(load) in project root of erlang application.
;;  `erlang-make'
;;    run make command at project root directory
;;  `erlang-compile-dwim'
;;    run make:all(load), if  Emakefile doesn't exists,call `erlang-compile' if with prefix `C-u' then run make.
;;  `erlang-dired-mode'
;;    Erlang application development minor mode.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'erlang)

(eval-when-compile
  (defvar comment-indent-hook)
  (defvar dabbrev-case-fold-search)
  (defvar tempo-match-finder)
  (defvar compilation-menu-map)
  (defvar next-error-last-buffer))

(eval-when-compile
  (if (or (featurep 'bytecomp)
          (featurep 'byte-compile))
      (progn
        (cond ((string-match "Lucid\\|XEmacs" emacs-version)
               (put 'comment-indent-hook 'byte-obsolete-variable nil)
               ;; Do not warn for unused variables
               ;; when compiling under XEmacs.
               (setq byte-compile-warnings
                     '(free-vars unresolved callargs redefine))))
        (require 'comint)
        (require 'tempo)
        (require 'compile))))

;;;###autoload
(defun erlang-create-project(root-dir)
  (interactive "Fselect a directory to create erlang project")
  (unless (file-exists-p root-dir)
    (make-directory root-dir))
  (unless (file-directory-p root-dir)
    (error "%s is not a directory!"  root-dir))
  (make-directory (expand-file-name  "src"  root-dir) t)
  (make-directory (expand-file-name  "include" root-dir) t)
  (make-directory (expand-file-name "ebin" root-dir) t)
  (make-directory (expand-file-name  "test" root-dir) t)
  (make-directory (expand-file-name  "deps" root-dir) t)
  (make-directory (expand-file-name  "priv" root-dir) t)
  (unless (or (file-exists-p (expand-file-name "Emakefile" root-dir))
              (file-exists-p  (expand-file-name "Emakefile" root-dir)))
    (with-temp-file (expand-file-name "Emakefile" root-dir)
      (insert "{ 'src/*', [debug_info, {i ,\"include/\"} ,{outdir,\"ebin/\"}] }.\n")
      (insert "{ 'test/*', [debug_info, {i ,\"src/\"}, {i ,\"include/\"}, {outdir,\"ebin\"} ] }.\n")
      ))

  (unless (or (file-exists-p (expand-file-name "makefile" root-dir))
              (file-exists-p  (expand-file-name "Makefile" root-dir)))
    (with-temp-file (expand-file-name "Makefile" root-dir)
      (insert "PREFIX:=../\n")
      (insert "DEST:=$(PREFIX)$(PROJECT)\n")
      (insert "REBAR=./rebar\n")
      (insert "\n")
      (insert "all:\n")
      (insert "	@$(REBAR) get-deps compile\n")
      (insert "edoc:\n")
      (insert "	@$(REBAR) doc\n")
      (insert "test:\n")
      (insert "	@rm -rf .eunit\n")
      (insert "	@mkdir -p .eunit\n")
      (insert "	@$(REBAR) skip_deps=true eunit\n")
      (insert "clean:\n")
      (insert "	@$(REBAR) clean\n")
      (insert "build_plt:\n")
      (insert "	@$(REBAR) build-plt\n")
      (insert "dialyzer:\n")
      (insert "	@$(REBAR) dialyze\n")
      (insert "app:\n")
      (insert "	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)\n")
      )
    )
  (dired root-dir)
  )

(defun erlang-root (&optional Emakefile)
  "Look for Emakefile file to find project root of erlang application."
  (let ((erlang-root (locate-dominating-file default-directory (or Emakefile  "Emakefile"))))
    (if erlang-root
        (expand-file-name erlang-root)
      nil)))

(defun erlang-root-by-makefile ()
  "Look for Makefile file to find project root of erlang application.
if found return the directory or nil
"
  (let ((erlang-root (locate-dominating-file default-directory "Makefile")))
    (if erlang-root
        (expand-file-name erlang-root)
      (setq erlang-root (locate-dominating-file default-directory "makefile"))
      (if erlang-root
          (expand-file-name erlang-root)
        nil
        ))))

;;;###autoload
(defun erlang-emake (arg)
  "run make:all(load) in project root of erlang application."
  (interactive "P")
  (let ((project-root (erlang-root)))
    (if (not project-root)
        (call-interactively 'erlang-compile)
      (save-some-buffers)
      (inferior-erlang-prepare-for-input)
      (let* (end)
        (with-current-buffer inferior-erlang-buffer
          (compilation-forget-errors))

        (inferior-erlang-send-command
         (format "cd(\"%s\")." project-root) nil)
        (sit-for 0)
        (inferior-erlang-wait-prompt)

        (inferior-erlang-send-command
         "make:all([load])." nil)
        (sit-for 0)
        (inferior-erlang-wait-prompt)

        (setq end (inferior-erlang-send-command
                   "cd(\"ebin/\")." nil))
        (sit-for 0)
        (inferior-erlang-wait-prompt)
        (with-current-buffer inferior-erlang-buffer
          (setq compilation-error-list nil)
          (set-marker compilation-parsing-end end))
        (setq compilation-last-buffer inferior-erlang-buffer))

      )
    )
  )

;;;###autoload
(defun erlang-make()
  "run make command at project root directory"
  (interactive)
  (let ((project-root ( erlang-root-by-makefile))
        (compile-command compile-command))
    (when project-root
      (setq compile-command (concat "make --directory=" project-root))
      )
    (call-interactively 'compile)
    )
  )

;;;###autoload
(defun erlang-compile-dwim(&optional arg)
  "run make:all(load), if  Emakefile doesn't exists,call `erlang-compile' if with prefix `C-u' then run make."
  (interactive "P")
  (if arg
      (call-interactively 'erlang-make)      ;`C-u'
    (call-interactively ' erlang-emake)
    )
  )

(defvar erlang-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") 'erlang-shell-display)
    (define-key map (kbd "C-c C-l") 'erlang-compile-display)
    (define-key map (kbd "C-c C-k") 'erlang-compile-dwim) ;run make:all([load])
    (define-key map (kbd "C-c C-p") 'erlang-create-project) ; you should bind this fun
    map))

;; (define-key erlang-dired-mode-map (kbd "C-z s") 'erlang-emake)
;;;###autoload
(define-minor-mode erlang-dired-mode
  "Erlang application development minor mode."
  nil
  " ErlDir"
  erlang-dired-mode-map)

;;;###autoload
(defun erlang-dired-mode-fun()
  (let ((erlang-project-root-dir (erlang-root)))
    (when erlang-project-root-dir  (erlang-dired-mode t))
    )
  )

;;;###autoload
(add-hook 'dired-mode-hook 'erlang-dired-mode-fun)

;;;###autoload
(defun erlang-mode-hook-1()
  (define-key erlang-mode-map (kbd "C-c C-k") 'erlang-compile-dwim) ;C-cC-k
  )
(add-hook 'erlang-mode-hook 'erlang-mode-hook-1)

(provide 'erlang-dired-mode)
;;; erlang-dired.el ends here

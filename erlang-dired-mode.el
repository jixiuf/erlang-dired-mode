;;; erlang-dired-mode.el --- erlang dired mode    -*- coding:utf-8 -*-

;; Description: erlang dired mode
;; Created: 2011-12-20 22:41
;; Last Updated: 纪秀峰 2012-11-25 13:16:37 星期日
;; Author: Joseph(纪秀峰)  jixiuf@gmail.com
;; Keywords: erlang dired Emakefile
;; URL: http://www.emacswiki.org/emacs/erlang-dired-mode.el
;; X-URL:git://github.com/jixiuf/erlang-dired-mode.git

;; Copyright (C) 2011,2012 纪秀峰, all rights reserved.

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

;; when you open dired mode buffer ,if a Emakefile are in current or parent
;; directory recursively , then `erlang-dired-mode' minor mode is enabled.
;;
;;  I write a function named `erlang-create-project' ,it will create a Emakefile
;;  and a Makefile(copide from mochiweb.) and erlang standard project .

;; `erlang-compile-dwim'
;;  if Emakefile exists in project root directory ,call "make:all[load]"
;;  if not call `erlang-compile'
;;  with  prefix `C-u':
;;  if Makefile exists in project root directory ,call make --directory project-root-directory
;;  if not call default make command (maybe ks "make -k")
;;
;;; Commands:
;;
;; Below are complete command list:
;;
;;  `erlang-export-current-function'
;;    export current function.
;;  `erlang-emake'
;;    run make:all(load) in project root of erlang application,if Emakefile doesn't exists ,call `erlang-compile' instead
;;  `erlang-make'
;;    run make command at project root directory
;;  `erlang-compile-dwim'
;;    call `erlang-emake', if with prefix `C-u' then run call `erlang-make'.
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
(defun erlang-export-current-function(&optional arg)
  "export current function.,with prefix `C-u' save `funname/argc' to king-ring."
  (interactive "P")
  (let ((init-pos (point-marker)) save-pos)
    (goto-char (car (bounds-of-thing-at-point 'defun)))
    (when (re-search-forward "(\\(.*?\\))") ;search params
      (let ((params (match-string 1))
            param-count funname fun-declare)
        (backward-sexp)
        (skip-chars-backward " \t")
        (setq funname (thing-at-point 'symbol))
        (if (string-match "^[ \t]*$" params)
            (setq param-count 0)
          (with-temp-buffer
            (insert params)
            (goto-char (point-min))
            (while (re-search-forward "{\\|\\[" (point-max) t)
              (forward-char -1)
              (kill-sexp)
              )
            (setq param-count (length  (split-string (buffer-string) ",")))
            )
          )
        (setq fun-declare (format "%s/%d" funname param-count))
        (cond
         ((and arg (equal arg '(4)))                         ; with C-u
          (message "copy function:%s to kill-ring" fun-declare)

          (kill-new  fun-declare)
          (cond
           ((progn (goto-char (point-min))(re-search-forward "[ \t]*-export[ \t]*([ \t]*\\[" (point-max) t))
            (looking-at "[ \t]*\\]"))
           ((progn (goto-char (point-min))(re-search-forward "[ \t]*-module[ \t]*(" (point-max) t))
            (end-of-line)
            (insert "\n-export([" fun-declare "]).\n")
            (goto-char (marker-position init-pos))
            )
           (t (goto-char (point-min))
              (insert "-export([" fun-declare "]).\n")
              (goto-char (marker-position init-pos))
              )
           )
          )
         ((and arg (equal arg '(16)))                         ; with C-uC-u
          (message "export function:%s" fun-declare)
          (goto-char (point-min))
          (if (re-search-forward "[ \t]*-export[ \t]*([ \t]*\\[" (point-max) t)
              (progn (beginning-of-line)(insert "-export([" fun-declare "]).\n"))
            (goto-char (point-min))
            (if (re-search-forward "[ \t]*-module[ \t]*(" (point-max) t)
                (progn
                  (end-of-line)
                  (insert "\n-export([" fun-declare "]).\n"))

              (goto-char (point-min))
              (insert "-export([" fun-declare "]).\n")
              ))
          (goto-char (marker-position init-pos))
          )
         (t
          (message "export function:%s" fun-declare)
          (goto-char (point-min))
          (if (re-search-forward "[ \t]*-export[ \t]*([ \t]*\\[" (point-max) t)
              (if (looking-at "[ \t]*\\]")
                  (insert fun-declare )
                (insert fun-declare ",")
                )
            (goto-char (point-min))
            (if (re-search-forward "[ \t]*-module[ \t]*(" (point-max) t)
                (progn
                  (end-of-line)
                  (insert "\n-export([" fun-declare "]).\n"))

              (goto-char (point-min))
              (insert "-export([" fun-declare "]).\n")
              ))
          (goto-char (marker-position init-pos))
          )
         )
        ))
    )
  )

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
  (unless (file-exists-p  (expand-file-name "Emakefile" root-dir))
    (with-temp-file (expand-file-name "Emakefile" root-dir)
      (insert "%% -*- erlang -*- \n")
      (insert "{ 'src/*', [debug_info, {i ,\"include/\"} , {i ,\"deps/\"} ,{outdir,\"ebin/\"}] }.\n")
      (insert "{ 'test/*', [debug_info, {i ,\"src/\"}, {i ,\"deps/\"},{i ,\"include/\"}, {outdir,\"ebin\"} ] }.\n")
      ))

  (unless (or (file-exists-p (expand-file-name "makefile" root-dir))
              (file-exists-p  (expand-file-name "Makefile" root-dir)))
    (with-temp-file (expand-file-name "Makefile" root-dir)
      (insert (format "PROJECT=%s\n"  (file-name-nondirectory root-dir)))
      (insert "PREVIOUS_RELEASE_VERSION=0.1")
      (insert "PREFIX:=../\n")
      (insert "DEST:=$(PREFIX)$(PROJECT)\n")
      (insert "REBAR=./rebar\n")
      (insert "REBAR_UP=../rebar\n")
      (insert ".PHONY:test\n")
      (insert "\n")
      (insert "compile:\n")
      (insert "	@$(REBAR) get-deps compile\n")
      (insert "	@-cp src/$(PROJECT).appup.src ebin/$(PROJECT).appup\n")
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
      (insert "	@$(REBAR) create-app dest=$(DEST) appid=$(PROJECT)\n\n")
      (insert "createnode:\n")
      (insert "	@-mkdir rel\n")
      (insert "	cd rel && $(REBAR_UP) create-node nodeid=$(PROJECT)\n")
      (insert "create-node:createnode\n")
      (insert "cn:createnode\n\n")
      (insert "generate:\n")
      (insert "	$(REBAR) generate\n")
      (insert "gen:generate\n")
      (insert "generatef:\n")
      (insert "	$(REBAR) generate -f\n\n")
      (insert "appup:\n")
      (insert "	$(REBAR) generate-appups previous_release=$(PROJECT)-$(PREVIOUS_RELEASE_VERSION)\n")
      (insert "upgrade:\n")
      (insert "	$(REBAR) generate-upgrade previous_release=$(PROJECT)-$(PREVIOUS_RELEASE_VERSION)\n")
      (insert "up:appup upgrade\n")
      )
    )
  (unless  (file-exists-p (expand-file-name "rebar.config" root-dir))
    (with-temp-file (expand-file-name "rebar.config" root-dir)
      (insert "%% -*- erlang -*-\n")
      (insert "%% {erl_opts, [debug_info,{src_dirs, ['src/']}, nowarn_unused_vars, nowarn_unused_function,native,{hipe,[o3]}]}.\n")
      (insert "{erl_opts, [debug_info]}.\n")
      (insert "{sub_dirs, [\"rel\"]}.\n")
      (insert "{lib_dirs,[\"..\"]}.\n")
      (insert "{deps, [  ]}.\n")
      (insert "{cover_enabled, true}.\n")
      (insert "{eunit_opts, [verbose, {report,{eunit_surefire,[{dir,\".\"}]}}]}.\n")
      )
    )
  (let* ((proj-root-name (file-name-nondirectory root-dir))
         (app_src  (expand-file-name (format "src/%s.app.src" proj-root-name) root-dir))
         )
    (unless  (file-exists-p app_src)
      (with-temp-file app_src
        (insert "%% -*- erlang -*-\n")
        (insert (format "{application, %s, \n" proj-root-name))
        (insert " [{description, \"desc\"},\n")
        (insert "  {vsn, \"0.1\"},\n")
        (insert "  {modules, []},\n")
        (insert "  {registered, []},\n")
        (insert (format "  {mod, {%s, []}},\n" proj-root-name))
        (insert "  {applications, [kernel, stdlib]},\n")
        (insert "  {env,[]}\n")
        (insert " ]}.\n")
        )
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
  "run make:all(load) in project root of erlang application,if Emakefile doesn't exists ,call `erlang-compile' instead"
  (interactive "P")
  (let ((project-root (erlang-root)))
    (if (not project-root)
        (call-interactively 'erlang-compile)
      (save-some-buffers)
      (dolist (filename (directory-files (expand-file-name "src/" project-root)))
        (cond
         ((string=  (file-name-extension filename) "app")
          (copy-file (expand-file-name (concat "src/" filename) project-root)
                     (expand-file-name (concat "ebin/" filename) project-root) t) )
         ((string=  (file-name-extension filename) "src")
          (copy-file (expand-file-name (concat "src/" filename) project-root)
                     (expand-file-name (concat "ebin/" (file-name-sans-extension  filename)) project-root) t) )))
      (inferior-erlang-prepare-for-input)
      (let* (end)
        (with-current-buffer inferior-erlang-buffer
          (compilation-forget-errors))

        (inferior-erlang-send-command
         (format "cd(\"%s\")." project-root) nil)
        (sit-for 0)
        (inferior-erlang-wait-prompt)

        (setq end (inferior-erlang-send-command
                   "make:all([load])." nil))
        (sit-for 0)
        (inferior-erlang-wait-prompt)
        (when (file-exists-p (expand-file-name "ebin" project-root))
          (setq end (inferior-erlang-send-command
                     "cd(\"ebin/\")." nil))  )
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
      ;; copy src/*.app.src to ebin/*.app
      (dolist (filename (directory-files (expand-file-name "src/" project-root)))
        (cond
         ((string=  (file-name-extension filename) "app")
          (copy-file (expand-file-name (concat "src/" filename) project-root)
                     (expand-file-name (concat "ebin/" filename) project-root) t) )
         ((string=  (file-name-extension filename) "src")
          (copy-file (expand-file-name (concat "src/" filename) project-root)
                     (expand-file-name (concat "ebin/" (file-name-sans-extension  filename)) project-root) t))))
      )
    (call-interactively 'compile)
    )
  )
;;;###autoload
(defun erlang-auto-compile()
  "auto compile current buffer to `project-root'/ebin when save"
  (interactive)
  (let ((project-root (or  (erlang-root) ""))
        (src (buffer-file-name))
        end)
    (when (string-match "\\.erl$" (buffer-name))
      (or (inferior-erlang-running-p)
          (save-excursion
            (inferior-erlang)))
      (with-current-buffer inferior-erlang-buffer
        (compilation-forget-errors))
      (setq end (inferior-erlang-send-command
                 (format "c('%s',[{d,debug},debug_info,{i,\"%sinclude\"},{i,\"%sdeps\"},{outdir, '%sebin'}])." src project-root project-root project-root)
                 nil))
      ;; (sit-for 0)
      ;; (inferior-erlang-wait-prompt)
      (with-current-buffer inferior-erlang-buffer
        (setq compilation-error-list nil)
        (set-marker compilation-parsing-end end))
      (setq compilation-last-buffer inferior-erlang-buffer)
      )
    ))

;;;###autoload
(defun erlang-compile-dwim(&optional arg)
  "call `erlang-emake', if with prefix `C-u' then run call `erlang-make'."
  (interactive "P")
  (if arg
      (call-interactively 'erlang-make)      ;`C-u'
    (call-interactively 'erlang-emake)
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

;;;###autoload(eval-after-load 'erlang '(add-hook 'dired-mode-hook 'erlang-dired-mode-fun))
(add-hook 'dired-mode-hook 'erlang-dired-mode-fun)

;;;###autoload
(defun erlang-mode-hook-1()
  (define-key erlang-mode-map (kbd "C-c C-k") 'erlang-compile-dwim) ;C-cC-k
  (add-hook 'after-save-hook 'erlang-auto-compile nil t)

  )
(add-hook 'erlang-mode-hook 'erlang-mode-hook-1)

(provide 'erlang-dired-mode)
;;; erlang-dired-mode.el ends here

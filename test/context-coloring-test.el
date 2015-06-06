;;; context-coloring-test.el --- Tests for context coloring  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; Tests for context coloring.

;; Use with `make test'.

;;; Code:

(require 'context-coloring)
(require 'ert-async)
(require 'js2-mode)


;;; Test running utilities

(defconst context-coloring-test-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defun context-coloring-test-read-file (path)
  "Return the file's contents from PATH as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name path context-coloring-test-path))
    (buffer-string)))

(defmacro context-coloring-test-with-fixture (fixture &rest body)
  "With the relative FIXTURE, evaluate BODY in a temporary
buffer."
  `(with-temp-buffer
     (progn
       (insert (context-coloring-test-read-file ,fixture))
       ,@body)))

(defun context-coloring-test-with-temp-buffer-async (callback)
  "Create a temporary buffer, and evaluate CALLBACK there.  A
teardown callback is passed to CALLBACK for it to invoke when it
is done."
  (let ((previous-buffer (current-buffer))
        (temp-buffer (generate-new-buffer " *temp*")))
    (set-buffer temp-buffer)
    (funcall
     callback
     (lambda ()
       (and (buffer-name temp-buffer)
            (kill-buffer temp-buffer))
       (set-buffer previous-buffer)))))

(defun context-coloring-test-with-fixture-async (fixture callback)
  "With the relative FIXTURE, evaluate CALLBACK in a temporary
buffer.  A teardown callback is passed to CALLBACK for it to
invoke when it is done."
  (context-coloring-test-with-temp-buffer-async
   (lambda (done-with-temp-buffer)
     (insert (context-coloring-test-read-file fixture))
     (funcall
      callback
      (lambda ()
        (funcall done-with-temp-buffer))))))


;;; Test defining utilities

(cl-defmacro context-coloring-test-define-deftest (name
                                                   &key mode
                                                   &key extension
                                                   &key no-fixture
                                                   &key async
                                                   &key post-colorization
                                                   &key enable-context-coloring-mode
                                                   &key get-args
                                                   &key before-each
                                                   &key after-each)
  "Define a deftest defmacro for tests prefixed with NAME. MODE
is called to set up tests' environments.  EXTENSION denotes the
suffix for tests' fixture files.  If NO-FIXTURE is non-nil, don't
use a fixture.  If ASYNC is non-nil, pass a callback to the
defined tests' bodies for them to call when they are done.  If
POST-COLORIZATION is non-nil, the tests run after
`context-coloring-colorize' finishes asynchronously.  If
ENABLE-CONTEXT-COLORING-MODE is non-nil, `context-coloring-mode'
is activated before tests.  GET-ARGS provides arguments to apply
to BEFORE-EACH, AFTER-EACH, and each tests' body, before and
after functions.  Functions BEFORE-EACH and AFTER-EACH run before
the major mode is activated before each test, and after each
test, even if an error is signaled."
  (declare (indent defun))
  (let ((macro-name (intern (format "context-coloring-test-deftest%s"
                                    (cond
                                     ;; No name means no dash.
                                     ((eq name nil) "")
                                     (t (format "-%s" name)))))))
    `(cl-defmacro ,macro-name (name
                               body
                               &key fixture
                               &key before
                               &key after)
       ,(format "Define a test for `%s' suffixed with NAME.

Function BODY makes assertions.
%s

Functions BEFORE and AFTER run before and after the test, even if
an error is signaled.

BODY is run after `context-coloring-mode' is activated, or after
initial colorization if colorization should occur."
                (cadr mode)
                (cond
                 (no-fixture "
There is no fixture, unless FIXTURE is specified.")
                 (t
                  (format "
The default fixture has a filename matching NAME (plus the
filetype extension, \"%s\"), unless FIXTURE is specified to
override it."
                          extension))))
       (declare (indent defun))
       ;; Commas in nested backquotes are not evaluated.  Binding the variables
       ;; here is probably the cleanest workaround.
       (let ((mode ,mode)
             (get-args ',(cond
                          (get-args get-args)
                          (t '(lambda () (list)))))
             (args (make-symbol "args"))
             (before-each ',before-each)
             (after-each ',after-each)
             (test-name (intern (format ,(format "%s-%%s"
                                                 (cond
                                                  (name)
                                                  (t "sync"))) name)))
             (fixture (cond
                       (fixture (format "./fixtures/%s" fixture))
                       (,no-fixture "./fixtures/empty")
                       (t (format ,(format "./fixtures/%%s.%s" extension) name)))))
         ,@(cond
            ((or async post-colorization)
             `((let ((post-colorization ,post-colorization))
                 `(ert-deftest-async ,test-name (done)
                    (let ((,args (funcall ,get-args)))
                      (context-coloring-test-with-fixture-async
                       ,fixture
                       (lambda (done-with-fixture)
                         (when ,before-each (apply ,before-each ,args))
                         (,mode)
                         (when ,before (apply ,before ,args))
                         (cond
                          (,post-colorization
                           (context-coloring-colorize
                            (lambda ()
                              (unwind-protect
                                  (progn
                                    (apply ,body ,args))
                                (when ,after (apply ,after ,args))
                                (when ,after-each (apply ,after-each ,args))
                                (funcall done-with-fixture))
                              (funcall done))))
                          (t
                           ;; Leave error handling up to the user.
                           (apply ,body (append
                                         (list (lambda ()
                                                 (when ,after (apply ,after ,args))
                                                 (when ,after-each (apply ,after-each ,args))
                                                 (funcall done-with-fixture)
                                                 (funcall done)))
                                         ,args)))))))))))
            (t
             `((let ((enable-context-coloring-mode ,enable-context-coloring-mode))
                 `(ert-deftest ,test-name ()
                    (let ((,args (funcall ,get-args)))
                      (context-coloring-test-with-fixture
                       ,fixture
                       (when ,before-each (apply ,before-each ,args))
                       (,mode)
                       (when ,before (apply ,before ,args))
                       (when ,enable-context-coloring-mode (context-coloring-mode))
                       (unwind-protect
                           (progn
                             (apply ,body ,args))
                         (when ,after (apply ,after ,args))
                         (when ,after-each (apply ,after-each ,args))))))))))))))

(context-coloring-test-define-deftest nil
  :mode 'fundamental-mode
  :no-fixture t)

(context-coloring-test-define-deftest async
  :mode 'fundamental-mode
  :no-fixture t
  :async t)

(context-coloring-test-define-deftest js
  :mode 'js-mode
  :extension "js"
  :post-colorization t)

(context-coloring-test-define-deftest js2
  :mode 'js2-mode
  :extension "js"
  :enable-context-coloring-mode t
  :before-each (lambda ()
                 (setq js2-mode-show-parse-errors nil)
                 (setq js2-mode-show-strict-warnings nil)))

(defmacro context-coloring-test-deftest-js-js2 (&rest args)
  "Simultaneously define the same test for js and js2."
  (declare (indent defun))
  `(progn
     (context-coloring-test-deftest-js ,@args)
     (context-coloring-test-deftest-js2 ,@args)))

(context-coloring-test-define-deftest emacs-lisp
  :mode 'emacs-lisp-mode
  :extension "el"
  :enable-context-coloring-mode t)

(context-coloring-test-define-deftest define-theme
  :mode 'fundamental-mode
  :no-fixture t
  :get-args (lambda ()
              (list (context-coloring-test-get-next-theme)))
  :after-each (lambda (theme)
                (setq context-coloring-maximum-face 7)
                (setq context-coloring-original-maximum-face
                      context-coloring-maximum-face)
                (disable-theme theme)
                (context-coloring-test-kill-buffer "*Warnings*")))


;;; Assertion functions

(defun context-coloring-test-get-last-message ()
  "Get the last message in the current messages bufffer."
  (let ((messages (split-string
                   (buffer-substring-no-properties
                    (point-min)
                    (point-max))
                   "\n")))
    (car (nthcdr (- (length messages) 2) messages))))

(defun context-coloring-test-assert-message (expected buffer)
  "Assert that message EXPECTED is at the end of BUFFER."
  (when (null (get-buffer buffer))
    (ert-fail
     (format
      (concat
       "Expected buffer `%s' to have message \"%s\", "
       "but the buffer did not have any messages.")
      buffer expected)))
  (with-current-buffer buffer
    (let ((message (context-coloring-test-get-last-message)))
      (when (not (equal message expected))
        (ert-fail
         (format
          (concat
           "Expected buffer `%s' to have message \"%s\", "
           "but instead it was \"%s\"")
          buffer expected
          message))))))

(defun context-coloring-test-assert-not-message (expected buffer)
  "Assert that message EXPECTED is not at the end of BUFFER."
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (let ((message (context-coloring-test-get-last-message)))
        (when (equal message expected)
          (ert-fail
           (format
            (concat
             "Expected buffer `%s' not to have message \"%s\", "
             "but it did")
            buffer expected)))))))

(defun context-coloring-test-assert-no-message (buffer)
  "Assert that BUFFER has no message."
  (when (get-buffer buffer)
    (ert-fail (format (concat "Expected buffer `%s' to have no messages, "
                              "but it did: `%s'")
                      buffer
                      (with-current-buffer buffer
                        (buffer-string))))))

(defun context-coloring-test-assert-error (body error-message)
  "Assert that BODY signals ERROR-MESSAGE."
  (let ((error-signaled-p nil))
    (condition-case err
        (progn
          (funcall body))
      (error
       (setq error-signaled-p t)
       (when (not (string-equal (cadr err) error-message))
         (ert-fail (format (concat "Expected the error \"%s\" to be thrown, "
                                   "but instead it was \"%s\".")
                           error-message
                           (cadr err))))))
    (when (not error-signaled-p)
      (ert-fail "Expected an error to be thrown, but there wasn't."))))


;;; Miscellaneous tests

(defun context-coloring-test-assert-trimmed (result expected)
  "Assert that RESULT is trimmed like EXPECTED."
  (when (not (string-equal result expected))
    (ert-fail "Expected string to be trimmed, but it wasn't.")))

(context-coloring-test-deftest trim
  (lambda ()
    (context-coloring-test-assert-trimmed (context-coloring-trim "") "")
    (context-coloring-test-assert-trimmed (context-coloring-trim " ") "")
    (context-coloring-test-assert-trimmed (context-coloring-trim "a") "a")
    (context-coloring-test-assert-trimmed (context-coloring-trim " a") "a")
    (context-coloring-test-assert-trimmed (context-coloring-trim "a ") "a")
    (context-coloring-test-assert-trimmed (context-coloring-trim " a ") "a")))

(context-coloring-test-deftest-async mode-startup
  (lambda (done)
    (js-mode)
    (add-hook
     'context-coloring-colorize-hook
     (lambda ()
       ;; If this runs we are implicitly successful; this test only confirms
       ;; that colorization occurs on mode startup.
       (funcall done)))
    (context-coloring-mode))
  :after (lambda ()
           ;; TODO: This won't run if there is a timeout.  Will probably have to
           ;; roll our own `ert-deftest-async'.
           (setq context-coloring-colorize-hook nil)))

(defmacro context-coloring-test-define-derived-mode (name)
  "Define a derived mode exclusively for tests."
  (let ((name (intern (format "context-coloring-test-%s-mode" name))))
    `(define-derived-mode ,name fundamental-mode "Testing")))

(context-coloring-test-define-derived-mode change-detection)

;; Simply cannot figure out how to trigger an idle timer; would much rather test
;; that.  But (current-idle-time) always returns nil in these tests.
(context-coloring-test-deftest-async change-detection
  (lambda (done)
    (context-coloring-define-dispatch
     'idle-change
     :modes '(context-coloring-test-change-detection-mode)
     :executable "node"
     :command "node test/binaries/noop")
    (context-coloring-test-change-detection-mode)
    (add-hook
     'context-coloring-colorize-hook
     (lambda ()
       (setq context-coloring-colorize-hook nil)
       (add-hook
        'context-coloring-colorize-hook
        (lambda ()
          (funcall done)))
       (insert " ")
       (set-window-buffer (selected-window) (current-buffer))
       (context-coloring-maybe-colorize (current-buffer))))
    (context-coloring-mode))
  :after (lambda ()
           (setq context-coloring-colorize-hook nil)))

(context-coloring-test-deftest check-version
  (lambda ()
    (when (not (context-coloring-check-version "2.1.3" "3.0.1"))
      (ert-fail "Expected version 3.0.1 to satisfy 2.1.3, but it didn't."))
    (when (context-coloring-check-version "3.0.1" "2.1.3")
      (ert-fail "Expected version 2.1.3 not to satisfy 3.0.1, but it did."))))

(context-coloring-test-deftest unsupported-mode
  (lambda ()
    (context-coloring-mode)
    (context-coloring-test-assert-message
     "Context coloring is not available for this major mode"
     "*Messages*")))

(context-coloring-test-deftest derived-mode
  (lambda ()
    (lisp-interaction-mode)
    (context-coloring-mode)
    (context-coloring-test-assert-not-message
     "Context coloring is not available for this major mode"
     "*Messages*")))

(context-coloring-test-define-derived-mode define-dispatch-error)

(context-coloring-test-deftest define-dispatch-error
  (lambda ()
    (context-coloring-test-assert-error
     (lambda ()
       (context-coloring-define-dispatch
        'define-dispatch-no-modes))
     "No mode defined for dispatch")
    (context-coloring-test-assert-error
     (lambda ()
       (context-coloring-define-dispatch
        'define-dispatch-no-strategy
        :modes '(context-coloring-test-define-dispatch-error-mode)))
     "No colorizer or command defined for dispatch")))

(context-coloring-test-define-derived-mode missing-executable)

(context-coloring-test-deftest missing-executable
  (lambda ()
    (context-coloring-define-dispatch
     'scopifier
     :modes '(context-coloring-test-missing-executable-mode)
     :command ""
     :executable "__should_not_exist__")
    (context-coloring-test-missing-executable-mode)
    (context-coloring-mode)))

(context-coloring-test-define-derived-mode unsupported-version)

(context-coloring-test-deftest-async unsupported-version
  (lambda (done)
    (context-coloring-define-dispatch
     'outta-date
     :modes '(context-coloring-test-unsupported-version-mode)
     :executable "node"
     :command "node test/binaries/outta-date"
     :version "v2.1.3")
    (context-coloring-test-unsupported-version-mode)
    (add-hook
     'context-coloring-check-scopifier-version-hook
     (lambda ()
       (unwind-protect
           (progn
             ;; Normally the executable would be something like "outta-date"
             ;; rather than "node".
             (context-coloring-test-assert-message
              "Update to the minimum version of \"node\" (v2.1.3)"
              "*Messages*"))
         (funcall done))))
    (context-coloring-mode))
  :after (lambda ()
           (setq context-coloring-check-scopifier-version-hook nil)))

(context-coloring-test-define-derived-mode disable-mode)

(context-coloring-test-deftest-async disable-mode
  (lambda (done)
    (let (torn-down)
      (context-coloring-define-dispatch
       'disable-mode
       :modes '(context-coloring-test-disable-mode-mode)
       :executable "node"
       :command "node test/binaries/noop"
       :teardown (lambda ()
                   (setq torn-down t)))
      (unwind-protect
          (progn
            (context-coloring-test-disable-mode-mode)
            (context-coloring-mode)
            (context-coloring-mode -1)
            (when (not torn-down)
              (ert-fail "Expected teardown function to have been called, but it wasn't.")))
        (funcall done)))))


;;; Theme tests

(defvar context-coloring-test-theme-index 0
  "Unique index for unique theme names.")

(defun context-coloring-test-get-next-theme ()
  "Return a unique symbol for a throwaway theme."
  (prog1
      (intern (format "context-coloring-test-theme-%s"
                      context-coloring-test-theme-index))
    (setq context-coloring-test-theme-index
          (+ context-coloring-test-theme-index 1))))

(defun context-coloring-test-assert-face (level foreground &optional negate)
  "Assert that a face for LEVEL exists and that its `:foreground'
is FOREGROUND, or the inverse if NEGATE is non-nil."
  (let* ((face (context-coloring-level-face level))
         actual-foreground)
    (when (not (or negate
                   face))
      (ert-fail (format (concat "Expected face for level `%s' to exist; "
                                "but it didn't")
                        level)))
    (setq actual-foreground (face-attribute face :foreground))
    (when (funcall (if negate 'identity 'not)
                   (string-equal foreground actual-foreground))
      (ert-fail (format (concat "Expected face for level `%s' "
                                "%sto have foreground `%s'; "
                                "but it %s.")
                        level
                        (if negate "not " "") foreground
                        (if negate
                            "did" (format "was `%s'" actual-foreground)))))))

(defun context-coloring-test-assert-not-face (&rest arguments)
  "Assert that LEVEL does not have a face with `:foreground'
FOREGROUND.  Apply ARGUMENTS to
`context-coloring-test-assert-face', see that function."
  (apply 'context-coloring-test-assert-face
         (append arguments '(t))))

(defun context-coloring-test-assert-theme-originally-set-p
    (settings &optional negate)
  "Assert that `context-coloring-theme-originally-set-p' returns
t for a theme with SETTINGS, or the inverse if NEGATE is
non-nil."
  (let ((theme (context-coloring-test-get-next-theme)))
    (put theme 'theme-settings settings)
    (when (funcall (if negate 'identity 'not)
                   (context-coloring-theme-originally-set-p theme))
      (ert-fail (format (concat "Expected theme `%s' with settings `%s' "
                                "%sto be considered to have defined a level, "
                                "but it %s.")
                        theme settings
                        (if negate "not " "")
                        (if negate "was" "wasn't"))))))

(defun context-coloring-test-assert-not-theme-originally-set-p (&rest arguments)
  "Assert that `context-coloring-theme-originally-set-p' does not
return t for a theme with SETTINGS.  Apply ARGUMENTS to
`context-coloring-test-assert-theme-originally-set-p', see that
function."
  (apply 'context-coloring-test-assert-theme-originally-set-p
         (append arguments '(t))))

(context-coloring-test-deftest theme-originally-set-p
  (lambda ()
    (context-coloring-test-assert-theme-originally-set-p
     '((theme-face context-coloring-level-0-face)))
    (context-coloring-test-assert-theme-originally-set-p
     '((theme-face face)
       (theme-face context-coloring-level-0-face)))
    (context-coloring-test-assert-theme-originally-set-p
     '((theme-face context-coloring-level-0-face)
       (theme-face face)))
    (context-coloring-test-assert-not-theme-originally-set-p
     '((theme-face face)))))

(defun context-coloring-test-assert-theme-settings-highest-level
    (settings expected-level)
  "Assert that a theme with SETTINGS has the highest level
EXPECTED-LEVEL."
  (let ((theme (context-coloring-test-get-next-theme)))
    (put theme 'theme-settings settings)
    (context-coloring-test-assert-theme-highest-level theme expected-level)))

(defun context-coloring-test-assert-theme-highest-level
    (theme expected-level &optional negate)
  "Assert that THEME has the highest level EXPECTED-LEVEL, or the
inverse if NEGATE is non-nil."
  (let ((highest-level (context-coloring-theme-highest-level theme)))
    (when (funcall (if negate 'identity 'not) (eq highest-level expected-level))
      (ert-fail (format (concat "Expected theme with settings `%s' "
                                "%sto have a highest level of `%s', "
                                "but it %s.")
                        (get theme 'theme-settings)
                        (if negate "not " "") expected-level
                        (if negate "did" (format "was %s" highest-level)))))))

(defun context-coloring-test-assert-theme-not-highest-level (&rest arguments)
  "Assert that THEME's highest level is not EXPECTED-LEVEL.
Apply ARGUMENTS to
`context-coloring-test-assert-theme-highest-level', see that
function."
  (apply 'context-coloring-test-assert-theme-highest-level
         (append arguments '(t))))

(context-coloring-test-deftest theme-highest-level
  (lambda ()
    (context-coloring-test-assert-theme-settings-highest-level
     '((theme-face foo))
     -1)
    (context-coloring-test-assert-theme-settings-highest-level
     '((theme-face context-coloring-level-0-face))
     0)
    (context-coloring-test-assert-theme-settings-highest-level
     '((theme-face context-coloring-level-1-face))
     1)
    (context-coloring-test-assert-theme-settings-highest-level
     '((theme-face context-coloring-level-1-face)
       (theme-face context-coloring-level-0-face))
     1)
    (context-coloring-test-assert-theme-settings-highest-level
     '((theme-face context-coloring-level-0-face)
       (theme-face context-coloring-level-1-face))
     1)))

(defun context-coloring-test-kill-buffer (buffer)
  "Kill BUFFER if it exists."
  (when (get-buffer buffer) (kill-buffer buffer)))

(defun context-coloring-test-deftheme (theme)
  "Dynamically define theme THEME."
  (eval (macroexpand `(deftheme ,theme))))

(context-coloring-test-deftest-define-theme additive
  (lambda (theme)
    (context-coloring-test-deftheme theme)
    (context-coloring-define-theme
     theme
     :colors '("#aaaaaa"
               "#bbbbbb"))
    (context-coloring-test-assert-no-message "*Warnings*")
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")))

(defun context-coloring-test-assert-defined-warning (theme)
  "Assert that a warning about colors already being defined for
theme THEME is signaled."
  (context-coloring-test-assert-message
   (format (concat "Warning (emacs): Context coloring colors for theme "
                   "`%s' are already defined")
           theme)
   "*Warnings*"))

(context-coloring-test-deftest-define-theme unintentional-override
  (lambda (theme)
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(context-coloring-level-0-face ((t (:foreground "#aaaaaa"))))
     '(context-coloring-level-1-face ((t (:foreground "#bbbbbb")))))
    (context-coloring-define-theme
     theme
     :colors '("#cccccc"
               "#dddddd"))
    (context-coloring-test-assert-defined-warning theme)
    (context-coloring-test-kill-buffer "*Warnings*")
    (enable-theme theme)
    (context-coloring-test-assert-defined-warning theme)
    (context-coloring-test-assert-face 0 "#cccccc")
    (context-coloring-test-assert-face 1 "#dddddd")))

(context-coloring-test-deftest-define-theme intentional-override
  (lambda (theme)
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(context-coloring-level-0-face ((t (:foreground "#aaaaaa"))))
     '(context-coloring-level-1-face ((t (:foreground "#bbbbbb")))))
    (context-coloring-define-theme
     theme
     :override t
     :colors '("#cccccc"
               "#dddddd"))
    (context-coloring-test-assert-no-message "*Warnings*")
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#cccccc")
    (context-coloring-test-assert-face 1 "#dddddd")))

(context-coloring-test-deftest-define-theme pre-recede
  (lambda (theme)
    (context-coloring-define-theme
     theme
     :recede t
     :colors '("#aaaaaa"
               "#bbbbbb"))
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(context-coloring-level-0-face ((t (:foreground "#cccccc"))))
     '(context-coloring-level-1-face ((t (:foreground "#dddddd")))))
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#cccccc")
    (context-coloring-test-assert-face 1 "#dddddd")))

(context-coloring-test-deftest-define-theme pre-recede-delayed-application
  (lambda (theme)
    (context-coloring-define-theme
     theme
     :recede t
     :colors '("#aaaaaa"
               "#bbbbbb"))
    (context-coloring-test-deftheme theme)
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")))

(context-coloring-test-deftest-define-theme post-recede
  (lambda (theme)
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(context-coloring-level-0-face ((t (:foreground "#aaaaaa"))))
     '(context-coloring-level-1-face ((t (:foreground "#bbbbbb")))))
    (context-coloring-define-theme
     theme
     :recede t
     :colors '("#cccccc"
               "#dddddd"))
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")))

(context-coloring-test-deftest-define-theme recede-not-defined
  (lambda (theme)
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(foo-face ((t (:foreground "#ffffff")))))
    (context-coloring-define-theme
     theme
     :recede t
     :colors '("#aaaaaa"
               "#bbbbbb"))
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")))

(context-coloring-test-deftest-define-theme unintentional-obstinance
  (lambda (theme)
    (context-coloring-define-theme
     theme
     :colors '("#aaaaaa"
               "#bbbbbb"))
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(context-coloring-level-0-face ((t (:foreground "#cccccc"))))
     '(context-coloring-level-1-face ((t (:foreground "#dddddd")))))
    (enable-theme theme)
    (context-coloring-test-assert-defined-warning theme)
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")))

(context-coloring-test-deftest-define-theme intentional-obstinance
  (lambda (theme)
    (context-coloring-define-theme
     theme
     :override t
     :colors '("#aaaaaa"
               "#bbbbbb"))
    (context-coloring-test-deftheme theme)
    (custom-theme-set-faces
     theme
     '(context-coloring-level-0-face ((t (:foreground "#cccccc"))))
     '(context-coloring-level-1-face ((t (:foreground "#dddddd")))))
    (enable-theme theme)
    (context-coloring-test-assert-no-message "*Warnings*")
    (context-coloring-test-assert-face 0 "#aaaaaa")
    (context-coloring-test-assert-face 1 "#bbbbbb")))

(defun context-coloring-test-assert-maximum-face (maximum &optional negate)
  "Assert that `context-coloring-maximum-face' is MAXIMUM, or the
inverse if NEGATE is non-nil."
  (when (funcall (if negate 'identity 'not)
                 (eq context-coloring-maximum-face maximum))
    (ert-fail (format (concat "Expected `context-coloring-maximum-face' "
                              "%sto be `%s', "
                              "but it %s.")
                      (if negate "not " "") maximum
                      (if negate
                          "was"
                        (format "was `%s'" context-coloring-maximum-face))))))

(defun context-coloring-test-assert-not-maximum-face (&rest arguments)
  "Assert that `context-coloring-maximum-face' is not MAXIMUM.
Apply ARGUMENTS to `context-coloring-test-assert-maximum-face',
see that function."
  (apply 'context-coloring-test-assert-maximum-face
         (append arguments '(t))))

(context-coloring-test-deftest-define-theme disable-cascade
  (lambda (theme)
    (let ((maximum-face-value 9999))
      (setq context-coloring-maximum-face maximum-face-value)
      (context-coloring-test-deftheme theme)
      (context-coloring-define-theme
       theme
       :colors '("#aaaaaa"
                 "#bbbbbb"))
      (let ((second-theme (context-coloring-test-get-next-theme)))
        (context-coloring-test-deftheme second-theme)
        (context-coloring-define-theme
         second-theme
         :colors '("#cccccc"
                   "#dddddd"
                   "#eeeeee"))
        (let ((third-theme (context-coloring-test-get-next-theme)))
          (context-coloring-test-deftheme third-theme)
          (context-coloring-define-theme
           third-theme
           :colors '("#111111"
                     "#222222"
                     "#333333"
                     "#444444"))
          (enable-theme theme)
          (enable-theme second-theme)
          (enable-theme third-theme)
          (disable-theme third-theme)
          (context-coloring-test-assert-face 0 "#cccccc")
          (context-coloring-test-assert-face 1 "#dddddd")
          (context-coloring-test-assert-face 2 "#eeeeee")
          (context-coloring-test-assert-maximum-face 2))
        (disable-theme second-theme)
        (context-coloring-test-assert-face 0 "#aaaaaa")
        (context-coloring-test-assert-face 1 "#bbbbbb")
        (context-coloring-test-assert-maximum-face 1))
      (disable-theme theme)
      (context-coloring-test-assert-not-face 0 "#aaaaaa")
      (context-coloring-test-assert-not-face 1 "#bbbbbb")
      (context-coloring-test-assert-maximum-face
       maximum-face-value))))


;;; Coloring tests

(defun context-coloring-test-assert-position-level (position level)
  "Assert that POSITION has LEVEL."
  (let ((face (get-text-property position 'face))
        actual-level)
    (when (not (and face
                    (let* ((face-string (symbol-name face))
                           (matches (string-match
                                     context-coloring-level-face-regexp
                                     face-string)))
                      (when matches
                        (setq actual-level (string-to-number
                                            (substring face-string
                                                       (match-beginning 1)
                                                       (match-end 1))))
                        (= level actual-level)))))
      (ert-fail (format (concat "Expected level at position %s, "
                                "which is \"%s\", to be %s; "
                                "but it was %s")
                        position
                        (buffer-substring-no-properties position (1+ position)) level
                        actual-level)))))

(defun context-coloring-test-assert-position-face (position face-regexp)
  "Assert that the face at POSITION satisfies FACE-REGEXP."
  (let ((face (get-text-property position 'face)))
    (when (or
           ;; Pass a non-string to do an `equal' check (against a symbol or nil).
           (unless (stringp face-regexp)
             (not (equal face-regexp face)))
           ;; Otherwise do the matching.
           (when (stringp face-regexp)
             (not (string-match-p face-regexp (symbol-name face)))))
      (ert-fail (format (concat "Expected face at position %s, "
                                "which is \"%s\", to be %s; "
                                "but it was %s")
                        position
                        (buffer-substring-no-properties position (1+ position)) face-regexp
                        face)))))

(defun context-coloring-test-assert-position-comment (position)
  "Assert that the face at POSITION is a comment."
  (context-coloring-test-assert-position-face
   position "\\`font-lock-comment\\(-delimiter\\)?-face\\'"))

(defun context-coloring-test-assert-position-constant-comment (position)
  "Assert that the face at POSITION is a constant comment."
  (context-coloring-test-assert-position-face position '(font-lock-constant-face
                                                         font-lock-comment-face)))

(defun context-coloring-test-assert-position-string (position)
  "Assert that the face at POSITION is a string."
  (context-coloring-test-assert-position-face position 'font-lock-string-face))

(defun context-coloring-test-assert-position-nil (position)
  "Assert that the face at POSITION is nil."
  (context-coloring-test-assert-position-face position nil))

(defun context-coloring-test-assert-coloring (map)
  "Assert that the current buffer's coloring matches MAP.

MAP's newlines should correspond to the current fixture.

The following characters appearing in MAP assert coloring for
corresponding points in the fixture:

0-9: Level equals number.
C: Face is constant comment.
c: Face is comment.
n: Face is nil.
s: Face is string.

Any other characters are discarded.  Characters \"x\" and any
other non-letters are guaranteed to always be discarded."
  ;; Omit the superfluous, formatting-related leading newline.  Can't use
  ;; `save-excursion' here because if an assertion fails it will cause future
  ;; tests to get messed up.
  (goto-char (point-min))
  (let* ((map (substring map 1))
         (index 0)
         char-string
         char)
    (while (< index (length map))
      (setq char-string (substring map index (1+ index)))
      (setq char (string-to-char char-string))
      (cond
       ;; Newline
       ((= char 10)
        (forward-line)
        (beginning-of-line))
       ;; Number
       ((and (>= char 48)
             (<= char 57))
        (context-coloring-test-assert-position-level
         (point) (string-to-number char-string))
        (forward-char))
       ;; 'C' = Constant comment
       ((= char 67)
        (context-coloring-test-assert-position-constant-comment (point))
        (forward-char))
       ;; 'c' = Comment
       ((= char 99)
        (context-coloring-test-assert-position-comment (point))
        (forward-char))
       ;; 'n' = nil
       ((= char 110)
        (context-coloring-test-assert-position-nil (point))
        (forward-char))
       ;; 's' = String
       ((= char 115)
        (context-coloring-test-assert-position-string (point))
        (forward-char))
       (t
        (forward-char)))
      (setq index (1+ index)))))

(context-coloring-test-deftest-js-js2 function-scopes
  (lambda ()
    (context-coloring-test-assert-coloring "
000 0 0 11111111 11 110
11111111 011 1
    111 1 1 22222222 22 221
    22222222 122 22
1")))

(context-coloring-test-deftest-js-js2 global
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxxxx () {
    111 1 1 00000001xxx11
}());")))

(context-coloring-test-deftest-js2 block-scopes
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxxxx () {
    11 111 2
        222 12
        222 22
    2
}());"))
  :before (lambda ()
            (setq context-coloring-js-block-scopes t))
  :after (lambda ()
           (setq context-coloring-js-block-scopes nil)))

(context-coloring-test-deftest-js-js2 catch
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxxxx () {
    111 11 22222 222 2
        222 1 2 22
        222 22 33333 333 3
            333 1 3 33
        3
    2
}());")))

(context-coloring-test-deftest-js-js2 key-names
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxxxx () {
    111111 1
        11 11
        1 1 1
    11
}());")))

(context-coloring-test-deftest-js-js2 property-lookup
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxxxx () {
    0000001111111
    0000001 111111
    00000011111111111
}());")))

(context-coloring-test-deftest-js-js2 key-values
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxxxx () {
    xxx x;
    (xxxxxxxx () {
        xxxxxx {
            x: 1
        };
    }());
}());")))

(context-coloring-test-deftest-js-js2 syntactic-comments-and-strings
  (lambda ()
    (context-coloring-test-assert-coloring "
0000 00
ccccccc
cccccccccc
ssssssssssss0"))
  :fixture "comments-and-strings.js")

(context-coloring-test-deftest-js-js2 syntactic-comments
  (lambda ()
    (context-coloring-test-assert-coloring "
0000 00
ccccccc
cccccccccc
0000000000000"))
  :fixture "comments-and-strings.js"
  :before (lambda ()
            (setq context-coloring-syntactic-strings nil))
  :after (lambda ()
           (setq context-coloring-syntactic-strings t)))

(context-coloring-test-deftest-js-js2 syntactic-strings
  (lambda ()
    (context-coloring-test-assert-coloring "
0000 00
0000000
0000000000
ssssssssssss0"))
  :fixture "comments-and-strings.js"
  :before (lambda ()
            (setq context-coloring-syntactic-comments nil))
  :after (lambda ()
           (setq context-coloring-syntactic-comments t)))

(context-coloring-test-deftest-js2 unterminated-comment
  ;; As long as `add-text-properties' doesn't signal an error, this test passes.
  (lambda ()))

(context-coloring-test-deftest-emacs-lisp defun
  (lambda ()
    (context-coloring-test-assert-coloring "
111111 000 1111 111 111111111 1111
  11 111 111 111 000011

0000 0 0 00

111111 01
111111 111")))

(context-coloring-test-deftest-emacs-lisp lambda
  (lambda ()
    (context-coloring-test-assert-coloring "
00000000 1111111 1111
           11111111 11 2222222 2222
                         222 22 12 2221 111 0 00")))

(context-coloring-test-deftest-emacs-lisp quote
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxx x (x)
  (xx (xx x 111
      111111 1 111 111
      111111 1 1111111111 11 111 1 111 1 00001 10000 11 00001 1 10000
                 sss ccc
                 1111

(xxxxxx '(sss cc
          sss cc
          ))

(xxxxxx () 111111 11111)")))

(context-coloring-test-deftest-emacs-lisp splice
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxxx ()
  111111 00001 100001)")))

(context-coloring-test-deftest-emacs-lisp comment
  (lambda ()
    ;; Just check that the comment isn't parsed syntactically.
    (context-coloring-test-assert-coloring "
(xxxxx x ()
  (xx (x xxxxx-xxxx xx)   cccccccccc
      11 00000-0000 11))) cccccccccc")))

(context-coloring-test-deftest-emacs-lisp string
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxx x (x)
  (xxxxxx x x sss 1 0 sssss 0 1 sssssss11")))

(context-coloring-test-deftest-emacs-lisp ignored
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxxxx x ()
  (x x 1 11 11 111 111 11 1 111 (1 1 1)))")))

(context-coloring-test-deftest-emacs-lisp let
  (lambda ()
    (context-coloring-test-assert-coloring "
1111 11
      cccccccccc
      11 sss1
      11 01
      11 00001
      11 2222 22
               22 02
               22 000022
           2222 2 2 2 00002211
  1111 1 1 1 000011")))

(context-coloring-test-deftest-emacs-lisp let*
  (lambda ()
    (context-coloring-test-assert-coloring "
11111 11
       11 11
       11 000011
  1111 1 1 1 0 0 00001
  22222 22
         22 12
         22 00002
         22 02
         22 222
    2222 1 1 2 2 2 000022
  1111 1 1 1 0 0 000011")))

(context-coloring-test-deftest-emacs-lisp cond
  (lambda ()
    (context-coloring-test-assert-coloring "
(xxx (x)
  11111
   11 11
   10000 11
   1111 1 00001 11
   11 11111 1 000011
   cc c
   sss1)")))

(defun context-coloring-test-insert-unread-space ()
  "Simulate the insertion of a space as if by a user."
  (setq unread-command-events (cons '(t . 32)
                                    unread-command-events)))

(defun context-coloring-test-remove-faces ()
  "Remove all faces in the current buffer."
  (remove-text-properties (point-min) (point-max) '(face nil)))

(context-coloring-test-deftest-emacs-lisp iteration
  (lambda ()
    (let ((context-coloring-elisp-sexps-per-pause 2))
      (context-coloring-colorize)
      (context-coloring-test-assert-coloring "
cc `CC' `CC'
(xxxxx x ())")
      (context-coloring-test-remove-faces)
      (context-coloring-test-insert-unread-space)
      (context-coloring-colorize)
      ;; Coloring is interrupted after the first "sexp" (the comment in this
      ;; case).
      (context-coloring-test-assert-coloring "
cc `CC' `CC'
nnnnnn n nnn"))))

(provide 'context-coloring-test)

;;; context-coloring-test.el ends here

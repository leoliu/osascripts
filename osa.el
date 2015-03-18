;;; osa.el --- OSA script wrapper                    -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.6.0
;; Keywords: OSA, languages, tools
;; Created: 2013-09-08

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

;; AppleScript Guide: search 'applescript' in
;;      http://developer.apple.com/library/mac/navigation
;;
;; JavaScript for Automation:
;;      https://developer.apple.com/videos/wwdc/2014/?id=403

;;; Code:

(defgroup osa nil
  "OSA scripts (AppleScript, JavaScript etc)."
  :group 'languages)

(defcustom osa-debug nil
  "Non-nil to log the OSA script string."
  :type 'boolean
  :group 'osa)

(defconst osa-lisp-start "#{"
  "String marking the start of lisp code.")

(defconst osa-lisp-end "}"
  "String marking the end of lisp code.")

(defvar osa-lisp-re
  (concat (regexp-quote osa-lisp-start)
          "\\(\\(?:.\\|\n\\)*?\\)\\(?:#\\(.\\)\\)?"
          (regexp-quote osa-lisp-end)))

(defun osa-parse-line (line)
  (let ((start 0)
        (lisp-code))
    (when (stringp line)
      (while (string-match osa-lisp-re line start)
        (push (condition-case err
                  (read (match-string 1 line))
                (error (error "%s: %s" (error-message-string err) line)))
              lisp-code)
        (setq start (match-beginning 0))
        ;; XXX: makes a new string every time
        (setq line (replace-match (concat "%" (or (match-string 2 line) "S"))
                                  nil nil line))))
    (if (consp lisp-code)
        (cons 'format (cons line (nreverse lisp-code)))
      line)))

(defun osa-parse-lines (lines)
  (mapcar #'osa-parse-line lines))

(defmacro osa-debug (form)
  (if osa-debug
      (let ((-value- (make-symbol "-value-")))
        `(let ((,-value- ,form))
           (message "DEBUG [%s]: \n%s"
                    (format-time-string "%Y-%m-%dT%T%z")
                    ,-value-)
           ,-value-))
    form))

(defun osa-build-script (lines)
  (let ((lines (osa-parse-lines lines)))
    (cond
     ((cl-every #'stringp lines) (mapconcat 'identity lines "\n"))
     ((not (cdr lines))          (car lines))
     (t                          `(mapconcat 'identity (list ,@lines) "\n")))))

;;;###autoload
(define-obsolete-function-alias 'applescript 'osa "2015-03-18")

;;;###autoload
(defmacro osa (&rest lines)
  "Like `do-applescript' but allow embedding lisp code.
The value of the lisp code is interpolated in the applescript
string using format control string `%S'. It can also be specified
by appending `#C' where C is one of the chars supported by
`format'. Examples: \"#{fill-column}\" and \"#{fill-column#x}\"."
  ;; Check doc-string of `do-applescript' to see why
  ;; `string-to-multibyte' is needed.
  `(do-applescript (osa-debug (string-to-multibyte ,(osa-build-script lines)))))

;;;###autoload
(defmacro osajs (&rest lines)
  "Like `osa' (which see) but use JavaScript instead."
  `(mac-osa-script (osa-debug (string-to-multibyte ,(osa-build-script lines)))
                   "JavaScript"))

(provide 'osa)
;;; osa.el ends here

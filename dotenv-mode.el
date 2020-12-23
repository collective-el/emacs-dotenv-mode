;;; dotenv-mode.el --- Major mode for .env files -*- lexical-binding: t; -*-

;; Author: Preetpal S. Sohal
;; URL: https://github.com/preetpalS/emacs-dotenv-mode
;; Version: 0.2.4
;; Package-Requires: ((emacs "24.3"))
;; License: GNU General Public License Version 3

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
;; Major mode for editing .env files (which are used for storing
;; environment variables).
;;
;; Usage:
;;
;; (require 'dotenv-mode) ; unless installed from a package
;; (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode

;;; Code:

(defgroup dotenv ()
  "Major mode for editing .env files."
  :group 'languages
  :prefix "dotenv-")

(defvar dotenv-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "\"'" table) ; ?' is a string delimiter
    (modify-syntax-entry ?\" "\"" table) ; ?\" is a string delimiter
    (modify-syntax-entry ?# "<" table)   ; ?# starts comments
    (modify-syntax-entry ?\n ">" table)  ; ?\n ends comments
    (modify-syntax-entry ?_ "_" table)   ; ?_ can be used in variable and command names
    (modify-syntax-entry ?\\ "\\" table) ; ?\\ is an escape sequence character
    (modify-syntax-entry ?$ "'" table)   ; ?$ is an expression prefix; Used in highlighting $VARIABLES, ${SUBSTITUTED_VARIABLES}, and $(substituted commands) embedded in double-quoted strings
    table))

;; Adapted from code generously donated by Fuco1 (https://github.com/Fuco1; see: https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html)
(defun dotenv-mode--match-variables-in-double-quotes (limit)
  "Match variables in double-quotes in `dotenv-mode'."
  (with-syntax-table dotenv-mode-syntax-table
    (catch 'done
      (while (re-search-forward
              ;; `rx' is cool, mkay.
              (rx (or line-start
                      (not (any "\\")))
                  (group "$")
                  (group
                   (or (and "{" (+? nonl) "}")
                       (and "(" (+? nonl) ")") ;; Added to support for interpolated command substitution syntax (like: "$(shell command)")
                       (and (+ (any alnum "_")))
                       (and (any "*" "@" "#" "?" "-" "$" "!" "0" "_")))))
              limit t)
        (let ((string-syntax (nth 3 (syntax-ppss))))
          (when (and string-syntax (= string-syntax 34))
            (throw 'done (point))))))))

(defvar dotenv-mode-keywords
  '(("\\(export\\)[[:space:]]+" . 1)
    ;; Adapted from code generously donated by Fuco1 (https://github.com/Fuco1; see: https://fuco1.github.io/2017-06-11-Font-locking-with-custom-matchers.html)
    (dotenv-mode--match-variables-in-double-quotes (1 'default t)
                                                   (2 font-lock-variable-name-face t))
    ("\\([[:alpha:]_]+[[:alnum:]_]*\\)[=]" 1 font-lock-variable-name-face)
    ("^\\([[:alpha:]_]+[[:alnum:]_]*\\)[:=]" 1 font-lock-variable-name-face)
    ("\$[[:alpha:]]+[[:alnum:]_]*" . font-lock-variable-name-face)
    ("\${[[:alpha:]]+[[:alnum:]_]*}" . font-lock-variable-name-face)
    ("\$([[:alpha:]]+[[:alnum:]_]*)" . font-lock-variable-name-face)))


(defun dotenv-mode-prase-line (envline)
  (if-let ((line (s-trim envline)))
      (if (and (stringp line)
	       (not (string-empty-p line))
	       (not (s-starts-with-p "#" line)))
	  (let ((entry (s-split-up-to "=" line 1)))
	    `(,(car entry)
	      ,(car (cdr entry)))))))


;;;autoload
(defun dotenv-mode-apply-line (envline)
  (interactive (list (thing-at-point 'line)))
  nil
  (if-let ((entry (dotenv-mode-prase-line envline)))
      (apply #'setenv entry)))


;;;autoload
(defun dotenv-mode-apply-all ()
  (interactive)
  (seq-filter (lambda (x) x)
	      (mapcar #'dotenv-mode-apply-line
		      (s-split "\n"
			       (buffer-substring-no-properties
				(point-min) (point-max)))))
  (message "dotenv applied from buffer!!"))


;;;###autoload
(define-derived-mode dotenv-mode prog-mode ".env"
  "Major mode for `.env' files."
  :abbrev-table nil
  :syntax-table dotenv-mode-syntax-table
  (define-key dotenv-mode-map "\C-c\C-l" 'dotenv-mode-apply-line)
  (define-key dotenv-mode-map "\C-c\C-c" 'dotenv-mode-apply-all)
  (setq-local font-lock-defaults '((dotenv-mode-keywords))))

;;;###autoload
(mapc (lambda (s) (add-to-list 'auto-mode-alist `(,s . dotenv-mode)))
      '(
        "\\.env\\'"
        "\\.env\\.example\\'"
        ))


(defvar dotenv-definition-line-rx
  (rx line-start

      (group (+ (or alpha "_" num)))
      "="
      (group (+ printing))
      line-end)
  "Regexp for dotenv style line.")


(defvar dotenv-file-path nil
  "Path to dotenv file.")

(defvar dotenv-original-process-environment (copy-sequence process-environment)
  "Original process environment variables.

This value is applied when reading another Dotenv file.
Therefore, the value of Dotenv does not continue to be set.")


(defvar dotenv-definition-line-max-position 1000000000
  "Maximum buffer size for Dotenv files.

Define it as insurance when a huge dotenv is read.")

(defun dotenv-trim-value (value-definition)
  "Trim the value part of dotenv.

Whitespace is processed and comments are removed."
  (string-match (rx line-start (group (+ (not "#"))) (? "#" (* anything)) line-end)
		value-definition)
  (s-trim (match-string 1 value-definition)))


(defun dotenv-format-entry (name-value)
  "Format one line of dotenv into a format that can be handled by process-environment.

Example::
  FOO=BAR"
  (let ((name (car name-value))
	(value (cdr name-value)))
    (format "%s=%s"
	    (string-as-unibyte (car name-value))
	    (dotenv-trim-value
	     (string-as-unibyte (cdr name-value))))))


(defun dotenv-search-forward-definition ()
  "Finds the dotenv definition from the current point position in the current buffer and returns it as a list of cons"
  (if-let ((pos (re-search-forward dotenv-definition-line-rx
				   dotenv-definition-line-max-position
				   t)))  ;; no error
      (cons
       (cons (match-string 1)  ;; name
	     (match-string 2)) ;; value
       (dotenv-search-forward-definition))))


(defun dotenv-get-definitions-from-buffer ()
  "Finds and returns the dotenv definition in the current buffer"
  (save-excursion
    (goto-char 0)
    (dotenv-search-forward-definition)))


(defmacro dotenv-with-open-file (file-path &rest body)
  "Finds and returns the dotenv definition in the specified file"
  `(let* ((buf (find-file-noselect ,file-path))
	  (resp (with-current-buffer buf
		  ,@body)))
     (kill-buffer buf)
     resp))


(defun dotenv-load ()
  (interactive)
  (let* ((buf (find-file-noselect dotenv-file-path))
	 (envs (with-current-buffer buf (dotenv-get-definitions-from-buffer))))
    (kill-buffer buf)

    (setq process-environment
	  (append envs dotenv-original-process-environment))))


(provide 'dotenv-mode)

;;; dotenv-mode.el ends here

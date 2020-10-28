;;; eldoc-server.el --- An Web Server for Emacs Lisp Docstring  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/eldoc-server.el
;; Package-Requires: ((emacs "25.1") (web-server "0.1.2"))
;; Keywords: help
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Access Emacs Lisp Docstring from Web

;;; Code:

(require 'web-server)

(defvar eldoc-server-host "0.0.0.0")
(defvar eldoc-server-port 3000)

(defun eldoc-server--end (proc code object)
  (let ((string (encode-coding-string (concat (json-encode object) "\n") 'utf-8)))
    (ws-response-header
     proc code
     '("content-type" . "application/json; charset=utf-8")
     (cons "content-length" (string-bytes string)))
    (process-send-string proc string)))

(defvar eldoc-server--server nil)

;;;###autoload
(defun eldoc-server-start ()
  "Start the eldoc server."
  (interactive)
  (when eldoc-server--server
    (message "[eldoc-server] The server is already running, restarting")
    (ws-stop eldoc-server--server)
    (setq eldoc-server--server nil))
  (setq eldoc-server--server
        (ws-start
         `(((:GET . ,(rx bos "/api/describe-symbol")) .
            eldoc-server--api-describe-symbol))
         eldoc-server-port
         nil
         :host eldoc-server-host))
  (message "[eldoc-server] Listening at http://%s:%s/"
           eldoc-server-host
           eldoc-server-port))

(defun eldoc-server--api-describe-symbol (req)
  (let ((headers (oref req headers))
        (proc (oref req process)))
    (message "[eldoc-server] HEADERS: %S" headers)
    (let ((symbol (assoc-default "symbol" headers)))
      (pcase symbol
        ('nil 
         (eldoc-server--end proc 400 '((error . "Missing the symbol parameter"))))
        (""
         (eldoc-server--end proc 400 '((error . "Missing value for the symbol parameter"))))
        ((guard (null (intern-soft symbol)))
         (eldoc-server--end proc 400 `((error . ,(format "The symbol named %S is not defined in this Emacs instance"
                                                         symbol)))))
        (_
         (and (get-buffer "*Help*") (kill-buffer "*Help*"))
         (describe-symbol (intern-soft symbol))       ; timeout, error
         (let ((docstring (with-current-buffer "*Help*"
                            (buffer-substring-no-properties (point-min) (point-max)))))
           (and (get-buffer "*Help*") (kill-buffer "*Help*"))
           (eldoc-server--end proc 200 `((result . ,docstring)))))))))

(provide 'eldoc-server)
;;; eldoc-server.el ends here

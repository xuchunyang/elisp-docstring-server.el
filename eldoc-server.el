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
(require 'json)

(defvar eldoc-server-host "0.0.0.0")
(defvar eldoc-server-port 3000)

(defconst eldoc-server--load-dir
  (file-name-directory
   (or load-file-name buffer-file-name))
  "The directory of the package.")

(defvar eldoc-server--server nil)

(defun eldoc-server--end (proc code object)
  (let ((string (encode-coding-string (concat (json-encode object) "\n") 'utf-8)))
    (ws-response-header
     proc code
     '("content-type" . "application/json; charset=utf-8")
     (cons "content-length" (string-bytes string)))
    (process-send-string proc string)))

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
            eldoc-server--api-describe-symbol)
           ((:GET . ,(rx bos "/" eos)) .
            eldoc-server--index)
           ((lambda (_req) t) .
            eldoc-server--404))
         eldoc-server-port
         nil
         :host eldoc-server-host))
  (message "[eldoc-server] Listening at http://%s:%s/"
           eldoc-server-host
           eldoc-server-port))

;; `httpd-etag'
(defun eldoc-server--etag (file)
  "Compute the ETag for FILE."
  (concat "\"" (substring (sha1 (prin1-to-string (file-attributes file))) -16)
          "\""))

(defun eldoc-server--send-file (req path type)
  (with-slots (headers process) req
    (let ((req-etag (alist-get :IF-NONE-MATCH headers))
          (etag (eldoc-server--etag path)))
      (if (equal req-etag etag)
          (ws-response-header process 304)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path)
          (ws-response-header
           process 200
           (cons "content-type" type)
           (cons "content-length" (buffer-size))
           ;; TODO Add Last-Modified
           ;; TODO Add Date (for ALL responses)
           (cons "etag" etag))
          (process-send-region process (point-min) (point-max)))))))

(defun eldoc-server--index (req)
  (eldoc-server--send-file
   req
   (expand-file-name "index.html" eldoc-server--load-dir)
   "text/html; charset=utf-8"))

(defun eldoc-server--404 (req)
  (let ((proc (oref req process))
        (body "<h1>404 Not Found</h1>"))
    (ws-response-header
     proc 404
     '("content-type" . "text/html; charset=utf-8")
     `("content-length" . ,(string-bytes body)))
    (process-send-string proc body)))

(defun eldoc-server--api-describe-symbol (req)
  (let ((headers (oref req headers))
        (proc (oref req process)))
    (message "[eldoc-server] HEADERS: %S" headers)
    (let ((symbol (assoc-default "symbol" headers)))
      (pcase symbol
        ('nil 
         (eldoc-server--end
          proc 400
          '((error . "Missing the symbol parameter"))))
        (""
         (eldoc-server--end
          proc 400
          '((error . "Missing value for the symbol parameter"))))
        ((guard (null (intern-soft symbol)))
         (eldoc-server--end
          proc 400
          `((error . ,(format
                       "The symbol named %S is not defined in this Emacs instance"
                       symbol)))))
        (_
         (and (get-buffer "*Help*") (kill-buffer "*Help*"))
         (describe-symbol (intern-soft symbol)) ; timeout, error
         (let ((docstring (with-current-buffer "*Help*"
                            (buffer-substring-no-properties (point-min) (point-max)))))
           (and (get-buffer "*Help*") (kill-buffer "*Help*"))
           (if (string= "" docstring)
               (eldoc-server--end
                proc 400
                `((error . ,(format "No documentation for %s" symbol)))))
           (eldoc-server--end proc 200 `((result . ,docstring)))))))))

(provide 'eldoc-server)
;;; eldoc-server.el ends here

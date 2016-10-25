;;; dot-dot.el --- Tiny window manager deals with one buffer

;; Author: Anan Mikami <lateau@gmail.com>
;; Created: 25 Oct 2016
;; Keywords: window buffer manager
;; Version: 0.0.1
;; URL: https://github.com/lateau/dot-dot.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; dot-dot.el will automatically set global key binding "C-. C-." when it doesn't mapped yet.
;; Use that key binding to show/hide window.
;;
;; You can also set your prefer key binding to dot-dot:toggle.
;;   (global-set-key (kbd "C-. C-1") 'dot-dot:toggle)
;;
;; Adapt other buffer: M-x dot-dot:change-buffer

;;; Code:

(defgroup dot-dot nil
  "dot-dot"
  :prefix "dot-dot-"
  :group 'applications)

(defcustom dot-dot:window-height-rate 20
  "Specify dot-dot window's height"
  :type 'integer
  :group 'dot-dot)

(defvar dot-dot:buffer "")

(defconst dot-dot:key-seq (kbd "C-. C-."))

(defun dot-dot:open ()
  (split-window-below
   (/ (* (- 100 (abs dot-dot:window-height-rate)) (window-height)) 100))
  (other-window 1)
  (switch-to-buffer (get-buffer dot-dot:buffer)))

(defun dot-dot:get-window ()
  (car (member-if
        '(lambda (x) (equal dot-dot:buffer (buffer-name (window-buffer x))))
        (window-list))))

(defun dot-dot:close ()
  (let ((target (dot-dot:get-window)))
    (when target
      (delete-window target))))

;;;###autoload
(defun dot-dot:change-buffer ()
  "Adapt new buffer to dot-dot window"
  (interactive)
  (setq dot-dot:buffer (completing-read
                        "Buffer: "
                        (mapcar '(lambda (x) (buffer-name x)) (buffer-list))
                        nil t "")))

;;;###autoload
(defun dot-dot:toggle ()
  "Toggle dot-dot window"
  (interactive)
  (when (string-blank-p dot-dot:buffer)
    (dot-dot:change-buffer))
  (if (dot-dot:get-window)
      (dot-dot:close)
    (dot-dot:open)))

;;;###autoload
(unless (lookup-key (current-global-map) dot-dot:key-seq)
  (global-set-key dot-dot:key-seq 'dot-dot:toggle))

(provide 'dot-dot)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; dot-dot.el ends here

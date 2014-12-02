;;; highlight-thing.el --- Minimalistic minor mode to highlight current thing under point.

;; Copyright (c) 2014 Felix Geller

;; Author: Felix Geller <fgeller@gmail.com>
;; Keywords: highlight thing symbol
;; URL: https://github.com/fgeller/highlight-thing.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Global minor mode to highlight the current thing under point. Uses built-in
;; thingatpt and hi-lock functionality to identify the thing under point and
;; highlight it. Does not require font-lock to be enabled as hi-lock falls back
;; to overlays.

(require 'thingatpt)

(defvar hlt-what-thing 'symbol "What kind of thing to highlight. (cf. `thing-at-point')")
(defvar hlt-last-thing nil "Last highlighted thing.")
(defvar hlt-last-buffer nil "Buffer where last thing was highlighted.")
(defvar hlt-highlight-delay-seconds 0.5 "Time to wait before highlighting thing at point.")
(defvar hlt-timer nil "Timer that triggers highlighting.")

(defun hlt-highlight-loop ()
  (cond (highlight-thing-mode (hlt-highlight-current-thing))
	(t (hlt-deactivate-highlight-thing))))

(defun hlt-deactivate-highlight-thing ()
  (hlt-remove-last-highlight)
  (when hlt-timer (cancel-timer hlt-timer)))

(defun hlt-thing-regexp (thing)
  (cond ((eq hlt-what-thing 'symbol) (concat "\\_<" (regexp-quote thing) "\\_>"))
	((eq hlt-what-thing 'word) (concat "\\<" (regexp-quote thing) "\\>"))
	(t (regexp-quote thing))))

(defun hlt-remove-last-highlight ()
  (when (and hlt-last-thing hlt-last-buffer (buffer-live-p hlt-last-buffer))
    (with-current-buffer hlt-last-buffer
      (hi-lock-unface-buffer (hlt-thing-regexp hlt-last-thing)))))

(defun hlt-should-highlight ()
  (not (eq major-mode 'minibuffer-inactive-mode)))

(defun hlt-highlight-current-thing ()
  (interactive)
  (let* ((thing (thing-at-point hlt-what-thing)))
    (hlt-remove-last-highlight)
    (when (and (hlt-should-highlight) thing)
      (highlight-regexp (hlt-thing-regexp thing))
      (setq hlt-last-buffer (current-buffer))
      (setq hlt-last-thing thing))))

;;;###autoload
(define-minor-mode highlight-thing-mode
  "Minor mode that highlights things at point"
  nil " hlt" nil
  :global t :group 'highlight-thing
  (setq hlt-timer (run-with-idle-timer hlt-highlight-delay-seconds t 'hlt-highlight-loop)))

(provide 'highlight-thing)

;;; highlight-thing.el ends here

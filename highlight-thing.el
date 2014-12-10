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

;; More information: https://github.com/fgeller/highlight-thing.el

(require 'thingatpt)

(defvar highlight-thing-what-thing 'symbol
  "What kind of thing to highlight. (cf. `thing-at-point')")

(defvar highlight-thing-last-thing nil
  "Last highlighted thing.")

(defvar highlight-thing-last-buffer nil
  "Buffer where last thing was highlighted.")

(defvar highlight-thing-delay-seconds 0.5
  "Time to wait before highlighting thing at point.")

(defvar highlight-thing-timer nil
  "Timer that triggers highlighting.")

(defvar highlight-thing-excluded-major-modes nil
  "List of major modes to exclude from highlighting.")

(defun highlight-thing-loop ()
  (cond (highlight-thing-mode (highlight-thing-do))
	(t (highlight-thing-deactivate))))

(defun highlight-thing-deactivate ()
  (highlight-thing-remove-last)
  (when highlight-thing-timer (cancel-timer highlight-thing-timer)))

(defun highlight-thing-regexp (thing)
  (cond ((eq highlight-thing-what-thing 'symbol) (concat "\\_<" (regexp-quote thing) "\\_>"))
	((eq highlight-thing-what-thing 'word) (concat "\\<" (regexp-quote thing) "\\>"))
	(t (regexp-quote thing))))

(defun highlight-thing-remove-last ()
  (when (and highlight-thing-last-thing
	     highlight-thing-last-buffer
	     (buffer-live-p highlight-thing-last-buffer))
    (with-current-buffer highlight-thing-last-buffer
      (hi-lock-unface-buffer (highlight-thing-regexp highlight-thing-last-thing)))))

(defun highlight-thing-should-highlight ()
  (and (not (minibufferp))
       (not (member major-mode highlight-thing-excluded-major-modes))))

(defun highlight-thing-do ()
  (interactive)
  (let* ((thing (thing-at-point highlight-thing-what-thing)))
    (highlight-thing-remove-last)
    (when (and (highlight-thing-should-highlight) thing)
      (highlight-regexp (highlight-thing-regexp thing))
      (setq highlight-thing-last-buffer (current-buffer))
      (setq highlight-thing-last-thing thing))))

;;;###autoload
(define-minor-mode highlight-thing-mode
  "Minor mode that highlights things at point"
  nil " hlt" nil
  :global t :group 'highlight-thing
  (setq highlight-thing-timer (run-with-idle-timer highlight-thing-delay-seconds t 'highlight-thing-loop)))

(provide 'highlight-thing)

;;; highlight-thing.el ends here

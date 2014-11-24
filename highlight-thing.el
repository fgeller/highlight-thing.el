;;; highlight-thing.el --- Minimalistic minor mode to highlight current thing under point.

;; Author: Felix Geller <fgeller@gmail.com>
;; Keywords: highlight thing symbol
;;
;; Requires `thingatpt' to identify symbol at point.
;;
;; Based on `highlight-symbol-mode', but uses hi-lock functionality rather than
;; relying on font-lock.

(require 'thingatpt)

(defvar hlt-last-thing nil "Last highlighted thing.")
(defvar hlt-last-buffer nil "Buffer where last thing was highlighted.")
(defvar hlt-highlight-delay-seconds 0.5 "Time to wait before highlighting thing at point.")
(defvar hlt-timer nil "Timer that triggers highlighting.")

(defun hlt-trigger-timer ()
  (when hlt-timer (cancel-timer hlt-timer))
  (when highlight-thing-mode
    (hlt-highlight-current-thing)
    (setq hlt-timer (run-with-idle-timer hlt-highlight-delay-seconds t 'hlt-trigger-timer))))

(defun hlt-thing-regexp (thing)
  (concat "\\_<" (regexp-quote thing) "\\_>"))

(defun hlt-remove-last-highlight ()
  (when (and hlt-last-thing hlt-last-buffer (buffer-live-p hlt-last-buffer))
    (with-current-buffer hlt-last-buffer
      (hi-lock-unface-buffer (hlt-thing-regexp hlt-last-thing)))))

(defun hlt-should-highlight ()
  (not (eq major-mode 'minibuffer-inactive-mode)))

(defun hlt-highlight-current-thing ()
  (interactive)
  (let* ((thing (thing-at-point 'symbol)))
    (when (and (hlt-should-highlight) thing)
      (hlt-remove-last-highlight)
      (highlight-regexp (hlt-thing-regexp thing))
      (setq hlt-last-buffer (current-buffer))
      (setq hlt-last-thing thing))))

(define-minor-mode highlight-thing-mode
  "Minor mode that highlights things at point"
  nil " hlt" nil
  :global t
  (hlt-trigger-timer))

(provide 'highlight-thing)

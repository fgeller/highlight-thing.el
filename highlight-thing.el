;;; highlight-thing.el --- Minimalistic minor mode to highlight current thing under point.

;; Author: Felix Geller <fgeller@gmail.com>
;; Keywords: highlight thing symbol
;;
;; Requires `thingatpt' to identify symbol at point.
;;
;; Based on `highlight-symbol-mode', but uses hi-lock functionality rather than
;; relying on font-lock.

(require 'thingatpt)

(defvar hlt-what-thing 'symbol "What kind of thing to highlight. (cf. `thing-at-point')")
(defvar hlt-last-thing nil "Last highlighted thing.")
(defvar hlt-last-buffer nil "Buffer where last thing was highlighted.")
(defvar hlt-highlight-delay-seconds 0.5 "Time to wait before highlighting thing at point.")
(defvar hlt-timer nil "Timer that triggers highlighting.")

(defun hlt-highlight-loop ()
  (cond (highlight-thing-mode (hlt-highlight-current-thing))
	(t (cancel-timer hlt-timer))))

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
    (when (and (hlt-should-highlight) thing)
      (hlt-remove-last-highlight)
      (highlight-regexp (hlt-thing-regexp thing))
      (setq hlt-last-buffer (current-buffer))
      (setq hlt-last-thing thing))))

(define-minor-mode highlight-thing-mode
  "Minor mode that highlights things at point"
  nil " hlt" nil
  :global t
  (setq hlt-timer (run-with-idle-timer hlt-highlight-delay-seconds t 'hlt-highlight-loop)))

(provide 'highlight-thing)

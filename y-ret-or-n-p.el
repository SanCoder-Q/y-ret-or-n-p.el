;;; y-ret-or-n-p.el --- Enable [RET] for fast agreement as yes when emacs querying.

;; Copyright (C) 2016 Jianming Qu

;; Author: Jianming Qu <sancoder.q@gmail.com>
;; Version: 1.0
;; Keywords: query, yes, no
;; URL: https://github.com/SanCoder-Q/y-ret-or-n-p.el

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;;
;; | Command       | You have to type                        |
;; |---------------+-----------------------------------------|
;; | yes-or-no-p   | 'yes' or 'no', followed by RET          |
;; | y-or-n-p      | 'y' or 'n', followed by RET             |
;; | y-ret-or-n-p  | RET to confirm straight away or 'n' RET |
;;
;; NOTE: this package aliases `yes-or-no-p' & `y-or-n-p' in order to
;; achieve this functionality

;;; Code:

(defun y-ret-or-n-p (prompt)
  "Ask user a question.
  Return t if answer is \"y\" or hit the <Ret> directly. 
  Return nil if answer is \"n\".
  This is inspired by the function y-or-n-p from subr.el.
  Warning: This function will increase the risk of confirming by mistake."
  (let ((answer 'recenter)
        (padded (lambda (prompt &optional dialog)
                  (let ((l (length prompt)))
                    (concat prompt
                            (if (or (zerop l) (eq ?\s (aref prompt (1- l))))
                                "" " ")
                            (if dialog "" "(y [RET] or n) "))))))
    (cond
     (noninteractive
      (setq prompt (funcall padded prompt))
      (let ((temp-prompt prompt))
        (while (not (memq answer '(act skip)))
          (let ((str (read-string temp-prompt)))
            (cond ((member str '("" "y" "Y")) (setq answer 'act))
                  ((member str '("n" "N")) (setq answer 'skip))
                  (t (setq temp-prompt (concat "Please answer y [RET] or n.  "
                                               prompt))))))))
     ((and (display-popup-menus-p)
           last-input-event
           (listp last-nonmenu-event)
           use-dialog-box)
      (setq prompt (funcall padded prompt t)
            answer (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
     (t
      (setq prompt (funcall padded prompt))
      (while
          (let* ((scroll-actions '(recenter scroll-up scroll-down
                                            scroll-other-window scroll-other-window-down))
                 (key
                  (let ((cursor-in-echo-area t))
                    (when minibuffer-auto-raise
                      (raise-frame (window-frame (minibuffer-window))))
                    (read-key (propertize (if (memq answer scroll-actions)
                                              prompt
                                            (concat "Please answer y [RET] or n.  "
                                                    prompt))
                                          'face 'minibuffer-prompt)))))
            (setq answer (lookup-key query-replace-map (vector key) t))
            (cond
             ((memq answer '(skip act exit)) nil)
             ((eq answer 'recenter)
              (recenter) t)
             ((eq answer 'scroll-up)
              (ignore-errors (scroll-up-command)) t)
             ((eq answer 'scroll-down)
              (ignore-errors (scroll-down-command)) t)
             ((eq answer 'scroll-other-window)
              (ignore-errors (scroll-other-window)) t)
             ((eq answer 'scroll-other-window-down)
              (ignore-errors (scroll-other-window-down)) t)
             ((or (memq answer '(exit-prefix quit)) (eq key ?\e))
              (signal 'quit nil) t)
             (t t)))
        (ding)
        (discard-input))))
    (let ((ret (memq answer '(act exit))))
      (unless noninteractive
        (message "%s%c" prompt (if ret ?y ?n)))
      ret)))

(defalias 'yes-or-no-p 'y-ret-or-n-p)
(defalias 'y-or-n-p 'y-ret-or-n-p)

(provide 'y-ret-or-n-p)

;;; y-ret-or-n-p.el ends here

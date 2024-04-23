;;; consult-extras.el --- Modal keybinding mode -*- lexical-binding: t -*-
;;
;; Filename: consult-extras.el
;; Description: A few extra consult sources
;; Author: David Feller
;; Package-Version: 0.0
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4") consult conn helpful)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A few extra consult sources
;;
;;; Code:

(require 'consult)
(require 'conn)
(require 'helpful)

(defvar consult--source-mark-ring
  (list :name "Mark Ring"
        :narrow ?l
        :category 'consult-location
        :items (lambda ()
                 (ignore-errors
                   (consult--mark-candidates
                    (cons (mark-marker) mark-ring))))
        :state 'consult--mark-state))

(defvar consult--source-global-mark-ring
  (list :name "Global Mark Ring"
        :narrow ?g
        :category 'consult-location
        :items (lambda ()
                 (ignore-errors
                   (consult--global-mark-candidates global-mark-ring)))
        :state 'consult--mark-state))

(defvar consult--source-unpop-ring
  (list :name "Unpop Mark Ring"
        :narrow ?u
        :category 'consult-location
        :items (lambda ()
                 (ignore-errors
                   (consult--mark-candidates conn--unpop-ring)))
        :state 'consult--mark-state))

(defvar consult--source-register-locations
  (list :name "Mark Registers"
        :narrow ?r
        :category 'consult-location
        :items (lambda ()
                 (let ((res nil))
                   (ignore-errors
                     (consult--global-mark-candidates
                      (dolist (cons register-alist res)
                        (when (markerp (cdr cons))
                          (push (cdr cons) res)))))))
        :state 'consult--mark-state))

(defvar consult-saved-locations-sources
  '(consult--source-mark-ring
    consult--source-unpop-ring
    consult--source-global-mark-ring
    consult--source-register-locations
    consult--source-bookmark))

(defun consult--mark-state ()
  (let ((jump-state
         (consult--state-with-return (consult--jump-preview) #'consult--jump)))
    (lambda (action cand)
      (funcall jump-state action (when cand (car (consult--get-location cand)))))))

(defun consult-all-marks ()
  (interactive)
  (let ((win (selected-window))
        (pt (point-marker)))
    (condition-case nil
        (consult--multi consult-saved-locations-sources
                        :require-match t
                        :prompt "Go to: "
                        :sort nil)
      (quit (with-selected-window win
              (set-window-buffer (selected-window) (marker-buffer pt))
              (goto-char pt))))))

(defun consult-symbol ()
  (interactive)
  (require 'helpful)
  (let (keymaps functions commands variables)
    (mapatoms
     (lambda (m)
       (cond ((commandp m)
	      (push (symbol-name m) commands))
             ((fboundp m)
	      (push (symbol-name m) functions))
             ((keymapp m)
	      (push (symbol-name m) keymaps))
             ((helpful--variable-p m)
	      (push (symbol-name m) variables)))))
    (consult--multi
     `((:name "Functions"
	      :narrow ?f
	      :category function
	      :items ,functions
	      :action ,(lambda (name)
                         (helpful-function (intern name))))
       (:name "Commands"
	      :narrow ?c
	      :category function
	      :items ,commands
	      :action ,(lambda (name)
                         (helpful-command (intern name))))
       (:name "Variables"
	      :narrow ?v
	      :category variable
	      :items ,variables
	      :action ,(lambda (name)
                         (helpful-variable (intern name))))
       (:name "Keymaps"
	      :narrow ?k
	      :category symbol
	      :items ,keymaps
	      :action ,(lambda (name)
                         (describe-keymap (intern name)))))
     :require-match t 
     :prompt "Describe: "
     :sort 'string<)))

(provide 'consult-extras)

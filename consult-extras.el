(require 'consult)
(require 'helpful)

(defvar consult--source-tab-bar
  `(:name "Tab Bar"
          :narrow ?t
          :category tab-bar
          :items ,(lambda ()
                    (mapcar (lambda (tab-name)
                              (alist-get 'name tab-name))
                            (tab-bar--tabs-recent)))
          :action ,#'tab-bar-switch-to-tab
          :new ,#'conn-tab-bar-new-named-tab))
(add-to-list 'consult-buffer-sources 'consult--source-tab-bar 'append)

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

(defun consult--keymaps ()
  (let (res)
    (mapatoms (lambda (ob)
                (when (and (boundp ob) (keymapp (symbol-value ob)))
                  (push (symbol-name ob) res))))
    res))

(defun consult--variables ()
  (let (res)
    (mapatoms (lambda (ob)
                (when (helpful--variable-p ob)
                  (push (symbol-name ob) res))))
    res))

(defun consult--functions ()
  (let (res)
    (mapatoms (lambda (ob)
                (when (and (functionp ob)
                           (not (commandp ob)))
                  (push (symbol-name ob) res))))
    res))

(defun consult--commands ()
  (let (res)
    (mapatoms (lambda (ob)
                (when (commandp ob)
                  (push (symbol-name ob) res))))
    res))

(defun consult-all-marks ()
  (interactive)
  (consult--multi consult-saved-locations-sources
                  :require-match t
                  :prompt "Go to: "
                  :sort nil))

(defun consult-apropos ()
  (interactive)
  (let (keymaps functions commands variables)
    (mapatoms
     (lambda (m)
       (cond ((commandp m)
              (push (symbol-name m) commands))
             ((functionp m)
              (push (symbol-name m) functions))
             ((keymapp m)
              (push (symbol-name m) keymaps))
             ((helpful--variable-p m)
              (push (symbol-name m) variables)))))
    (consult--multi `((:name "Commands"
                             :narrow ?c
                             :category function
                             :items ,commands
                             :action ,(lambda (name)
                                        (helpful-function (intern name))))
                      (:name "Functions"
                             :narrow ?f
                             :category function
                             :items ,functions
                             :action ,(lambda (name)
                                        (helpful-function (intern name))))
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
                    :prompt "Go to: "
                    :sort nil)))

(provide 'consult-extras)

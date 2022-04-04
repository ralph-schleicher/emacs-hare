;;; hare.el --- a TortoiseSVN clone for Dired buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ralph Schleicher

;; Author: Ralph Schleicher <rs@ralph-schleicher.de>
;; Keywords: vc, files
;; Version: α

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General
;; Public License along with this program.  If not,
;; see <https://www.gnu.org/licenses/>.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'vc-svn)
(require 'vc-dir)
(require 'dired)
(require 'subr-x)
(require 'wid-edit)
(require 'cus-edit)
(require 'shell)

(defgroup hare nil
  "HareSVN is a TortoiseSVN clone for Dired buffers."
  :prefix "hare-"
  :group 'vc
  :group 'dired)

(defsubst hare--vc-state (file-name)
  "Return the HareSVN VC state of FILE-NAME.

Value is the VC state symbol as returned by the ‘vc-state’ function,
or ‘locked’ if the VC state indicates that the file is locked by some
other user."
  (when-let* ((backend (or vc-dir-backend
			   (ignore-errors
			     (vc-responsible-backend file-name))))
	      (state (vc-state-refresh file-name backend)))
    (if (stringp state) 'locked state)))

(defconst hare--vc-state-alist
  '((up-to-date
     :help "up-to-date"
     :flag ? )
    (edited
     :help "edited"
     :flag ?M) ;modified
    (locked
     :help "locked"
     :flag ?L)
    (needs-update
     :help "needs update"
     :flag ?O) ;outdated
    (needs-merge
     :help "needs merge"
     :flag ?P) ;patch, pull request
    (unlocked-changes
     :help "unlocked changes"
     :flag ?U)
    (added
     :help "added"
     :flag ?A)
    (removed
     :help "removed"
     :flag ?D) ;deleted
    (conflict
     :help "conflict"
     :flag ?C)
    (missing
     :help "missing"
     :flag ?V) ;void
    (ignored
     :help "ignored"
     :flag ?I)
    (unregistered
     :help "unregistered"
     :flag ??)
    (nil
     :help "no information"
     :flag ?-))
  "Alist of HareSVN properties associated with a VC state.
List elements are cons cells of the form ‘(STATE . PROPERTIES)’.
Key STATE is the VC state symbol as returned by the ‘vc-state’
 function, or ‘locked’ if the VC state indicates that the file
 is locked by some other user.
Value PROPERTIES is a property list.")

(defconst hare--vc-states (mapcar #'car hare--vc-state-alist)
  "List of VC state symbols.")

(defcustom hare-display-icons t
  "Whether or not to visualize VC states with the help of graphic icons."
  :type 'boolean
  :group 'hare)

(defsubst hare--display-graphic-p ()
  "Return true if VC states are visualized with the help of graphic icons."
  (and hare-display-icons (display-graphic-p)))

(defvar hare-icon-directory
  (let ((file-name (or load-file-name (locate-library "hare") buffer-file-name)))
    (file-name-as-directory
     (if (stringp file-name)
	 (expand-file-name "icons" (file-name-directory file-name))
       (expand-file-name "hare/icons" user-emacs-directory))))
  "Directory containing graphic icons.")

(defvar hare-icon-types '(svg png xpm)
  "List of potentially available image types for graphic icons.
The elements should be in descending order of preference.")

(defcustom hare-icon-margin 1
  "Amount of extra space to be added around an graphic icon.
This user option tunes the displayed icon size.

Consider running the command ‘hare-update-icons’ so that changes
are actually applied when redisplaying a buffer."
  :type 'integer
  :group 'hare)

(defvar hare--icon-alist ()
  "Alist of graphic icons.
List elements are cons cells of the form ‘(STATE . IMAGE)’.
Key STATE is the VC state symbol as returned by the ‘vc-state’
 function, or ‘locked’ if the VC state indicates that the file
 is locked by some other user.
Value IMAGE is an image descriptor.")

(defun hare--update-icon-alist ()
  "Update variable ‘hare--icon-alist’."
  (setq hare--icon-alist
	(when (display-graphic-p)
	  (let* ((width* (* 2 (default-font-width))) ;an even number
		 (height* (default-font-height))
		 (ascent* (aref (font-info (face-font 'default)) 8))
		 (margin* (max 0 hare-icon-margin))
		 ;; The actual icon size.
		 (size (max 0 (if (<= width* height*)
				  (- width* (* 2 margin*))
				(- height* (* 2 margin*)
				   (if (cl-oddp height*) 1 0)))))
		 (margin (/ (- width* size) 2))
		 ;; Vertical position.
		 (ascent (floor (* 100 (min 1 (/ (- ascent* margin)
						 (float size)))))))
	    (let ((image-load-path (cons 'hare-icon-directory image-load-path)))
	      (mapcar (lambda (state)
			(cons state (find-image
				     (nconc
				      (mapcan (lambda (type)
						(when (image-type-available-p type)
						  `((:type ,type
						     :file ,(format "%s-%d.%s" state size type)
						     :margin (,margin . 0)
						     :ascent ,ascent))))
					      hare-icon-types)
				      (mapcan (lambda (type)
						(when (image-type-available-p type)
						  `((:type ,type
						     :file ,(format "%s.%s" state type)
						     :margin (,margin . 0)
						     :ascent ,ascent
						     :width ,size))))
					      hare-icon-types)))))
		      hare--vc-states))))))
(cl-eval-when (:load-toplevel :execute)
  (hare--update-icon-alist))

;;;###autoload
(defun hare-update-icons ()
  "Update graphic icons."
  (interactive)
  ;; Remove current icons from the image cache.
  (dolist (cell hare--icon-alist)
    (when-let ((file-name (cl-getf (cddr cell) :file)))
      (clear-image-cache file-name)))
  (hare--update-icon-alist)
  ())

(defun hare--insert-image (spec &optional string text-properties)
  "Insert an graphic icon into the current buffer at point.
First argument SPEC is either a VC state, an image descriptor,
 or a list of image specifications."
  (let* ((state (cond ((symbolp spec) spec)
		      ((stringp spec) 'locked)
		      (t t))) ;not nil
	 (prop (cdr (assq state hare--vc-state-alist)))
	 (icon (if (consp spec)
		   (if (eq (car spec) 'image)
		       spec
		     (let ((image-load-path (cons 'hare-icon-directory image-load-path)))
		       (find-image spec)))
		 (cdr (assq state hare--icon-alist))))
	 (mark (point)))
    (insert (or string "  "))
    (when-let ((help (cl-getf prop :help)))
      (put-text-property mark (point) 'help-echo help))
    (when text-properties
      (add-text-properties mark (point) text-properties))
    (when icon
      (add-text-properties
       mark (point) `(display ,icon
		      rear-nonsticky t
		      cursor-intangible t)))
    icon))

;;;###autoload
(defun hare-list-vc-states ()
  (interactive)
  (hare-update-icons)
  (with-help-window "*VC States*"
    (with-current-buffer standard-output
      (erase-buffer)
      (insert "List of HareSVN version control states.\n\n")
      (dolist (cell hare--vc-state-alist)
	(let* ((state (car cell))
	       (prop (cdr cell))
	       (icon (cdr (assq state hare--icon-alist))))
	  (insert ?\s)
	  (let ((mark (point)))
	    (insert (cl-getf prop :flag))
	    (put-text-property mark (point) 'help-echo (cl-getf prop :help)))
	  (when (display-graphic-p)
	    (insert ?\s)
	    (hare--insert-image icon nil `(help-echo ,(cl-getf prop :help))))
	  (insert ?\s (symbol-name state) ?\n)))
      (setq cursor-type nil)
      (setq truncate-lines t))))

;;;; Dired Buffers

(defcustom hare-dired-hide-vc-headers "^\\(working directory\\):"
  "Whether or not to hide VC header lines in Dired buffers.
A value of t means to omit the header lines, nil means to display the
header lines.  A regular expression means to remove matching header
lines."
  :type '(choice (const :tag "All" t)
		 (const :tag "None" nil)
		 (regexp :tag "Lines matching"))
  :group 'hare)

;; Fix compiler warning.
(defvar hare--svn-menu)

(defun hare--dired-pop-up-menu ()
  "Pop-up the HareSVN menu."
  (interactive)
  (popup-menu (cl-case vc-dir-backend
		(SVN hare--svn-menu)
		(t   vc-menu-map))))

(defvar hare--dired-icon-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'hare--dired-pop-up-menu)
    (define-key map [mouse-2] 'hare--dired-pop-up-menu)
    (define-key map [mouse-3] 'hare--dired-pop-up-menu)
    map)
  "Keymap for HareSVN icons.")

(defun hare--dired-after-readin ()
  "Enrich the Dired buffer with HareSVN data."
  (set (make-local-variable 'vc-dir-backend)
       (ignore-errors
	 (vc-responsible-backend default-directory)))
  (let ((buffer-read-only nil)
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      ;; Insert the VC header lines.
      (when (and (not (buffer-narrowed-p)) ;i.e. ‘dired-readin’, not ‘dired-insert-subdir’
		 vc-dir-backend (not (eq hare-dired-hide-vc-headers t)))
	;; See ‘dired-subdir-alist’.
	(insert-before-markers
	 "Version control system: " (format "%s" vc-dir-backend) ?\n
	 ;; The VC working directory information is redundant since
	 ;; it is equal to the top-level Dired directory header.
	 "Working directory: " (abbreviate-file-name default-directory) ?\n
	 (if-let ((headers (vc-call-backend vc-dir-backend
					    'dir-extra-headers
					    default-directory)))
	     (concat (replace-regexp-in-string " +:" ":" headers) "\n")
	   "")
	 ?\n)
	(when (stringp hare-dired-hide-vc-headers)
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (point-min) (1- (point)))
	      (goto-char (point-min))
	      (while (re-search-forward hare-dired-hide-vc-headers nil t)
		(delete-region (save-excursion
				 (goto-char (match-beginning 0))
				 (forward-line 0)
				 (point))
			       (progn
				 (forward-line 1)
				 (point)))))))
	(if (= (point-min) (1- (point)))
	    (delete-region (point-min) (point))
	  ;; Some headers remain.  Protect them from Dired font lock
	  ;; changes.  Add support for ‘dired-hide-details-mode’, too.
	  (set-text-properties (point-min) (point)
			       '(font-lock-face default
				 invisible dired-hide-details-information))
	  ;; Highlight the header names.
	  (let ((limit (point)))
	    (goto-char (point-min))
	    (while (re-search-forward "^[^:]+:" limit t)
	      (add-text-properties (match-beginning 0) (match-end 0)
				   `(face ,dired-header-face
				     font-lock-face ,dired-header-face)))
	    (goto-char limit))))
      ;; Insert the HareSVN icons.
      (while (not (eobp))
	(when (dired-move-to-filename)
	  (let* ((file (dired-get-filename nil t))
      		 (state (and file vc-dir-backend (hare--vc-state file))))
	    (if (hare--display-graphic-p)
		(hare--insert-image
		 state nil `(keymap ,hare--dired-icon-keymap))
	      (let ((prop (cdr (assq state hare--vc-state-alist)))
		    (mark (point)))
		(insert (cl-getf prop :flag))
		(add-text-properties
		 mark (point) `(help-echo ,(cl-getf prop :help)
				keymap ,hare--dired-icon-keymap))))
	    (insert ?\s)))
	(forward-line 1)))))

;;;###autoload
(define-minor-mode hare-dired-mode
  "Toggle display of version control information in current Dired buffer.
When this minor mode is enabled, version control information such as
revision number and status are visualized."
  :group 'hare
  (unless (derived-mode-p 'dired-mode)
    (error "Not a Dired buffer"))
  (cond (hare-dired-mode
	 (add-hook 'dired-after-readin-hook 'hare--dired-after-readin t t))
	(t
	 (remove-hook 'dired-after-readin-hook 'hare--dired-after-readin t)))
  (dired-revert))

;;;; Forms

(defvar hare--temp-buffer-name "*HareSVN*"
  "The buffer name for temporary HareSVN buffers.")

(defvar hare--temp-buffer-action '(display-buffer-below-selected)
  "The buffer display action for temporary HareSVN buffers.")

(defvar hare--temp-buffer-from nil
  "The buffer from which a temporary HareSVN buffer originates.")
(put 'hare--temp-buffer-from 'permanent-local t)

(defun hare--temp-buffer-undertaker ()
  "Function to run when a temporary HareSVN buffer is killed."
  (when (buffer-live-p hare--temp-buffer-from)
    (save-current-buffer
      (pop-to-buffer hare--temp-buffer-from)))
  (delete-windows-on (current-buffer))
  (set-buffer-modified-p nil))

(defun hare--temp-buffer-window (&optional buffer-name buffer-action)
  "Create a temporary HareSVN buffer and show it in a window.
Does not change the selected window or the current buffer.

Return value is the window displaying the buffer.  The buffer
itself is empty."
  (let ((came-from (current-buffer)))
    (with-current-buffer-window
	(or buffer-name hare--temp-buffer-name)
	(or buffer-action hare--temp-buffer-action)
	(lambda (window _buffer)
	  ;; Return the window object.
	  window)
      (set (make-local-variable 'hare--temp-buffer-from) came-from)
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (set (make-local-variable 'kill-buffer-hook)
	   '(hare--temp-buffer-undertaker))
      (current-buffer))))

(defun hare--widget-value (widget)
  "Like ‘widget-value’ but return nil if WIDGET is not a widget."
  (when (widgetp widget)
    (widget-value widget)))

(defvar hare--form-horizontal-line (make-string 72 ?─)
  "Horizontal line object.

Candidates for horizontal line characters are ‘─’ (U+2500, box drawings
light horizontal) and ‘━’ (U+2501, box drawings heavy horizontal).")

(defun hare--form-horizontal-line ()
  "Insert a horizontal line."
  (cond ((stringp hare--form-horizontal-line)
	 (insert hare--form-horizontal-line ?\n))
	((characterp hare--form-horizontal-line)
	 (insert (make-string (- (window-text-width) 2) hare--form-horizontal-line) ?\n))))

(defvar hare--form-special '(default-directory)
  "List of special variables for form buffers.")

;; List of widget names, i.e. symbols binding a widget.
(defvar hare--form-widget-names)

(defmacro hare--form (widget-names documentation submit-form &rest body)
  "A simple form, i.e. dialog box, framework.

First argument WIDGET-NAMES is a list of symbols.
Second argument DOCUMENTATION is the documentation string.
Third argument SUBMIT-FORM is the submit button call-back form.

A form buffer is a temporary HareSVN buffer.  The form buffer is
initialized as follows.

   * The variables listed in ‘hare--form-special’ are copied to local
     variables in the form buffer.  These variables are also available
     in the environment of the submit button call-back form.

   * Local variables for the symbols listed in WIDGET-NAMES are created
     and initialized with nil.  These variables can be bound to widgets
     in the body of the form.  The values of these widgets are then
     available in the submit button call-back form.

   * A local keymap is installed in the form buffer.  The parent of
     this keymap is ‘widget-keymap’ so that the user can tab through
     the widgets of the form.

   * The documentation string is inserted at the beginning of the form
     buffer.

   * A cancel and submit push button is inserted.  Both buttons destory
     the form buffer.  After the form buffer is destroyed, the submit
     button evaluates SUBMIT-FORM in an environment where the values of
     the form's named widgets are bound to variables of the same name.
     The variables listed in ‘hare--form-special’ are also available in
     this environment.

   * The window containing the form buffer is selected and point,
     i.e. the input focus, is on the submit push button."
  (declare (indent 3))
  (let ((values (gensym "values"))
	(window (gensym "window"))
	(buffer (gensym "buffer")))
    `(let* ((,values (mapcar #'symbol-value hare--form-special))
	    (,window (hare--temp-buffer-window "*HareSVN Form*"))
	    (,buffer (window-buffer ,window)))
       (select-window ,window)
       (set-buffer ,buffer)
       ;; Forms are editable.
       (setq buffer-read-only nil)
       ;; Use the widget style of the customization engine.
       (custom--initialize-widget-variables)
       ;; Propagate the special variables.
       (set (make-local-variable 'hare--form-special-symbols) hare--form-special)
       (set (make-local-variable 'hare--form-special-values) ,values)
       (cl-mapc (lambda (symbol value)
		  (set (make-local-variable symbol) value))
		hare--form-special-symbols hare--form-special-values)
       ;; Bind the variables for the widget names.
       (set (make-local-variable 'hare--form-widget-names) ',widget-names)
       (dolist (widget-name hare--form-widget-names)
	 (set (make-local-variable widget-name) nil))
       ;; Prepare the local keymap.
       (let ((map (make-sparse-keymap)))
	 (set-keymap-parent map widget-keymap)
	 (use-local-map map))
       ;; Prepare the form.
       (let ((inhibit-modification-hooks t))
	 ;; Insert the documentation string.
	 (insert ,documentation ?\n ?\n)
	 ;; Insert the cancel and submit buttons.  There is an empty
	 ;; line before and after these buttons.
	 (insert ?\s)
	 (let* ((button (hare--form-quit-button " Cancel "))
       		(fire (lambda ()
       			(interactive)
       			(widget-item-action button))))
           (local-set-key (kbd "C-c C-q") fire)
           (local-set-key (kbd "C-c q") fire))
	 (insert ?\s ?\s)
	 (let* ((button (hare--form-quit-button "   OK   "
       			  ,@(when submit-form (list submit-form))))
       		(fire (lambda ()
       			(interactive)
       			(widget-item-action button))))
           (local-set-key (kbd "C-c C-c") fire)
           (local-set-key (kbd "C-c c") fire))
	 (insert ?\n ?\n)
	 (hare--form-horizontal-line)
	 ;; The body form.
	 ,@body
	 ,@(when body
             '((unless (bolp)
       		 (insert ?\n))
               (hare--form-horizontal-line))))
       (set-buffer-modified-p nil)
       ;; Prepare entry fields.
       (widget-setup)
       ;; Set focus on submit button.
       (goto-char (point-min))
       (widget-move 2)
       ())))

(defmacro hare--form-quit-button (label &rest body)
  "Insert a push button into the form.
When the button is pressed, the form will be destroyed.

First argument LABEL is the value of the push button.
The BODY is evaluated in an environment where the values
 of the form's named widgets are bound to variables of the
 same name."
  (declare (indent 1))
  `(widget-create 'push-button
                  :notify (lambda (&rest _ignore)
			    ,(if (null body)
				 '(kill-buffer (current-buffer))
			       `(let ((buffer hare--temp-buffer-from)
				      (call-back (cl-list* 'lambda (nconc (copy-sequence
									   hare--form-widget-names)
									  hare--form-special-symbols)
							   ',body))
				      (arguments (nconc (mapcar #'hare--widget-value
								(mapcar #'symbol-value
									hare--form-widget-names))
							hare--form-special-values)))
			          (kill-buffer (current-buffer))
				  (when (buffer-live-p buffer)
				    (set-buffer buffer))
			          (apply call-back arguments))))
                  ,@(when label (list label))))

(defun hare--form-check-list (list-of-strings checked)
  "Insert a check list into the form.

First argument LIST-OF-STRINGS are the check list items.
Second argument CHECKED determines the initial state of
 the check list items."
  (let (widget)
    (unless (bolp)
      (insert ?\n))
    (insert "Check: ")
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
                             (dolist (button (widget-get widget :buttons))
                               (unless (widget-value button)
                                 (widget-checkbox-action button))))
                   " All ")
    (insert ?\s)
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
                             (dolist (button (widget-get widget :buttons))
                               (when (widget-value button)
                                 (widget-checkbox-action button))))
                   " None ")
    (insert ?\n)
    (insert ?\n)
    (setq widget (apply #'widget-create 'checklist
			:entry-format " %b %v"
                        (mapcar (lambda (string)
                                  `(item ,string))
                                list-of-strings)))
    (let ((flag (not (null checked))))
      (dolist (button (widget-get widget :buttons))
        (unless (eq (widget-value button) flag)
          (widget-checkbox-action button))))
    widget))

(defun hare--form-svn-widget (type &rest options)
  "Insert a Subversion widget into the form.
Return value is the widget handle."
  (declare (indent 1))
  ;; See https://svnbook.red-bean.com/en/1.7/svn.ref.svn.html.
  (cl-case type
    (accept
     (apply #'widget-create 'menu-choice
	    :value 'postpone
	    :tag "Conflict Resolution"
	    :format "%t %[ Value Menu %]: %v"
	    :help-echo "Define the action for automatic conflict resolution"
	    `((const
	       :value postpone
	       :format "%t\n%d"
	       :menu-tag "Postpone"
	       :doc "\
Take no resolution action at all and instead allow the conflicts to be
recorded for future resolution.")
	      (const
	       :value edit
	       :format "%t\n%d"
	       :menu-tag "Edit"
	       :doc "\
Open each conflicted file in a text editor for manual resolution of
line-based conflicts.")
	      (const
	       :value launch
	       :format "%t\n%d"
	       :menu-tag "Launch"
	       :doc "\
Launch an interactive merge conflict resolution tool for each conflicted
file.")
	      (const
	       :value base
	       :format "%t\n%d"
	       :menu-tag "Base"
	       :doc "\
Choose the file that was the (unmodified) ‘BASE’ revision before you
tried to integrate changes from the server into your working copy.")
	      (const
	       :value working
	       :format "%t\n%d"
	       :menu-tag "Working"
	       :doc "\
Assuming that you've manually handled the conflict resolution, choose
the version of the file as it currently stands in your working copy.")
	      (const
	       :value mine-full
	       :format "%t\n%d"
	       :menu-tag "Mine, Full"
	       :doc "\
Resolve conflicted files by preserving all local modifications and
discarding all changes fetched from the server during the operation
which caused the conflict.")
	      (const
	       :value theirs-full
	       :format "%t\n%d"
	       :menu-tag "Theirs, Full"
	       :doc "\
Resolve conflicted files by discarding all local modifications and
integrating all changes fetched from the server during the operation
which caused the conflict.")
	      (const
	       :value mine-conflict
	       :format "%t\n%d"
	       :menu-tag "Mine, Conflict"
	       :doc "\
Resolve conflicted files by preferring local modifications over the
changes fetched from the server in conflicting regions of each file's
content.")
	      (const
	       :value theirs-conflict
	       :format "%t\n%d"
	       :menu-tag "Theirs, Conflict"
	       :doc "\
Resolve conflicted files by preferring the changes fetched from the
server over local modifications in conflicting regions of each file's
content."))))
    (auto-props
     (apply #'widget-create 'menu-choice
	    :value 'undefined
	    :tag "Automatic Property Assignment"
	    :format "%t %[ Value Menu %]: %v"
	    :help-echo "Override the runtime configuration directive for automatic property assignment"
	    `((const
	       :value undefined
	       :tag "not set"
	       :format "%t\n%h"
	       :menu-tag "Unset"
	       :doc "Apply the runtime configuration setting.
See the ‘enable-auto-props’ runtime configuration directive.")
	      (const
	       :value t
	       :tag "enabled"
	       :format "%t\n%h"
	       :menu-tag "Enable"
	       :doc "Enable automatic property assignment.
Override the ‘enable-auto-props’ runtime configuration directive.")
	      (const
	       :value nil
	       :tag "disabled"
	       :format "%t\n%h"
	       :menu-tag "Disable"
	       :doc "Disable automatic property assignment.
Override the ‘enable-auto-props’ runtime configuration directive."))))
    ((depth set-depth)
     (let ((set-depth (eq type 'set-depth)))
       ;; See https://svnbook.red-bean.com/en/1.7/svn.advanced.sparsedirs.html.
       (apply #'widget-create 'menu-choice
	      :tag (if set-depth "Set Sticky Depth" "Operational Depth")
	      :value (plist-get options :value)
	      :format "%t %[ Value Menu %]: %v"
	      :help-echo (if set-depth
			     "Change the scope (sticky depth) of a directory in the working copy"
			   "Limit the scope of the operation")
	      `((const
		 :value nil
		 :format "%t\n%d"
		 :menu-tag "None"
		 :doc ,(if set-depth
			   "Don't change the scope (sticky depth) of a directory in the working copy."
			 "Apply the default behavior of the operation."))
		(const
		 :value empty
		 :format "%t\n%d"
		 :menu-tag "Empty"
		 :doc "\
Include only the immediate target of the operation, not any of its file
or directory children.")
		(const
		 :value files
		 :format "%t\n%d"
		 :menu-tag "Files"
		 :doc "\
Include the immediate target of the operation and any of its immediate
file children.")
		(const
		 :value immediates
		 :format "%t\n%d"
		 :menu-tag "Immediates"
		 :doc "\
Include the immediate target of the operation and any of its immediate
file or directory children.  The directory children will themselves be
empty.")
		(const
		 :value infinity
		 :format "%t\n%d"
		 :menu-tag "Infinity"
		 :doc "\
Include the immediate target of the operation, its file and directory
children, its children's children, and so on to full recursion.")
		,@(when set-depth
		    '((const
		       :value exclude
		       :format "%t\n%d"
		       :menu-tag "Exclude"
		       :doc "\
Exclude the immediate target of the operation from the working copy.")))))))
    (revision
     ;; See https://svnbook.red-bean.com/en/1.7/svn.tour.revs.specifiers.html.
     (apply #'widget-create 'menu-choice
	    :tag "Revision"
	    :format "%t %[ Value Menu %]: %v"
	    :help-echo "Specify a revision"
	    `((const
	       :value nil
	       :format "%t\n%h"
	       :menu-tag "None"
	       :doc "The revision is not specified.")
	      (integer
	       :value 1
	       :format "%{%t%}: %v\n%h"
	       :tag "Number"
	       :size 10
	       :valid-regexp "\\`[1-9][0-9]*\\'"
	       :doc "The revision number.")
	      (string
	       :value ,(format-time-string "{%+4Y-%m-%d}" (time-add (current-time) (* 24 60 60)))
	       :format "%{%t%}: %v\n%h"
	       :tag "Date"
	       :size 32 ;{YYYY-MM-DD hh:mm:ss.sss±hh:mm}
	       :valid-regexp "\\`{[ 0-9TZ.:+-]+}\\'"
	       :doc "The most recent revision in the repository as of that date.")
	      (const
	       :value HEAD
	       :format "%t\n%h"
	       :doc "The latest revision in the repository.")
	      (const
	       :value BASE
	       :format "%t\n%h"
	       :doc "The revision of an item in the working copy.")
	      (const
	       :value COMMITTED
	       :format "%t\n%h"
	       :doc "The revision of an item's last commit before or at ‘BASE’.")
	      (const
	       :value PREV
	       :format "%t\n%h"
	       :doc "The revision before ‘COMMITTED’."))))
    ;; Generic widgets.
    (toggle
     ;; Should provide a :tag and optional :doc option.
     (apply #'widget-create 'toggle
	    :format (if (plist-get options :doc)
			"%t: %[ %v %]\n%h"
		      "%t: %[ %v %]\n")
	    options))
    (checkbox
     ;; Should provide a :tag and/or :doc option.
     (apply #'widget-create 'checkbox
	    :format (if (plist-get options :doc)
			(if (plist-get options :tag)
			    " %[%v%] %t\n%h"
			  " %[%v%] %h")
		      " %[%v%] %t\n")
	    options))
    ))

;;;; Process Buffer

(defcustom hare-delete-process-window t
  "Whether or not to delete the process window after an operation.
This option only has an effect if the process succeeds."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (set :tag "Conditions"
		      ;; Subversion commands.
		      (const svn-add)
		      (const svn-auth)
		      (const svn-blame)
		      (const svn-cat)
		      (const svn-changelist)
		      (const svn-checkout)
		      (const svn-cleanup)
		      (const svn-commit)
		      (const svn-copy)
		      (const svn-delete)
		      (const svn-diff)
		      (const svn-export)
		      (const svn-help)
		      (const svn-import)
		      (const svn-info)
		      (const svn-list)
		      (const svn-lock)
		      (const svn-log)
		      (const svn-merge)
		      (const svn-mergeinfo)
		      (const svn-mkdir)
		      (const svn-move)
		      (const svn-patch)
		      (const svn-propdel)
		      (const svn-propedit)
		      (const svn-propget)
		      (const svn-proplist)
		      (const svn-propset)
		      (const svn-relocate)
		      (const svn-resolve)
		      (const svn-resolved)
		      (const svn-revert)
		      (const svn-status)
		      (const svn-switch)
		      (const svn-unlock)
		      (const svn-update)
		      (const svn-upgrade)))
  :group 'hare)

(defcustom hare-delete-process-window-delay 3.0
  "Time to wait in seconds before the process window is deleted."
  :type 'number
  :group 'hare)

(defun hare--delete-process-window (buffer conditions)
  "Delete the process window.

First argument BUFFER is the process buffer.
Second argument CONDITIONS is a list of symbols.

This function acts according to the current values of the options
‘hare-delete-process-window’ and ‘hare-delete-process-window-delay’."
  (when (cond ((consp hare-delete-process-window)
	       (cl-some (lambda (condition)
			  (memq condition hare-delete-process-window))
			conditions))
	      (hare-delete-process-window))
    (sit-for (max 0.2 hare-delete-process-window-delay))
    (delete-windows-on buffer)))

(cl-defmacro hare--with-process-window ((buffer-var &optional conditions) &body body)
  "Pop-up the HareSVN process window and evaluate BODY.
If BODY returns non-nil, delete the process window."
  (declare (indent 1))
  (let ((buffer (gensym "buffer"))
	(status (gensym "status")))
    `(let* ((,buffer (window-buffer (hare--temp-buffer-window "*HareSVN Process*")))
	    ,@(when buffer-var `((,buffer-var ,buffer)))
	    (,status (progn ,@body)))
       (when ,status
	 (hare--delete-process-window ,buffer ,conditions)
	 (message "HareSVN process succeeded"))
       ,status)))

(defmacro hare--all-null (&rest forms)
  "Return non-nil if all forms return nil.
Without any form, value is true."
  `(cl-every #'null (list ,@forms)))

(defmacro hare--not-any-null (&rest forms)
  "Return non-nil if all forms return non-nil.
Without any form, value is true."
  `(not (cl-some #'null (list ,@forms))))

(defun hare--string (object)
  "Return a string described by OBJECT."
  (cl-typecase object
    (string
     object)
    (character
     (string object))
    (symbol
     (symbol-name object))
    (t
     (with-output-to-string
       (princ object)))))

(defun hare--string-equal (string1 string2 &optional ignore-case)
  "Return non-nil if STRING1 is equal to STRING2 in lexicographic order.
Case is not significant if optional argument IGNORE-CASE is non-nil."
  (let ((ans (compare-strings string1 0 nil string2 0 nil ignore-case)))
    (eq ans t)))

(defun hare--string-lessp (string1 string2 &optional ignore-case)
  "Return non-nil if STRING1 is less than STRING2 in lexicographic order.
Case is not significant if optional argument IGNORE-CASE is non-nil."
  (let ((ans (compare-strings string1 0 nil string2 0 nil ignore-case)))
    (and (numberp ans) (minusp ans))))

(defvar hare--file-name-ignore-case (memq system-type '(ms-dos windows-nt))
  "Whether or not to ignore case when comparing file names.")

(defun hare--file-name-equal (name1 name2)
  "Like ‘hare--string-equal’ but consider ‘hare--file-name-ignore-case’."
  (hare--string-equal name1 name2 hare--file-name-ignore-case))

(defun hare--file-name-lessp (name1 name2)
  "Like ‘hare--string-lessp’ but consider ‘hare--file-name-ignore-case’."
  (hare--string-lessp name1 name2 hare--file-name-ignore-case))

;;;; Subversion

(defun hare--svn-collect-paths (&rest options)
  "Collect paths, i.e. files and directories, for a Subversion command.
Signal an error if not within a working copy.

Return value is a list of the form ‘(ROOT PARENT CHILDREN)’.

First element ROOT is the top-level working copy directory.
Second element PARENT is the deepest version controlled working copy
 directory containing CHILDREN.
Third element CHILDREN is the list of child items.

All file names are absolute."
  (let (root parent children implicit-child)
    ;; Collect absolute path names.  Expand file name abbreviations,
    ;; like ‘~/foo’, and mark directory file names as directories.
    (cond ((derived-mode-p 'dired-mode)
	   (setq parent (file-name-as-directory (expand-file-name default-directory))
		 children (delq nil (dired-map-over-marks
				     ;; Expand ‘.’ and ‘..’ and mark directory
				     ;; file names as directories.
				     (when-let* ((relative (dired-get-filename t t))
						 (absolute (expand-file-name relative parent)))
				       (if (and (not (file-symlink-p absolute)) (file-directory-p absolute))
					   (file-name-as-directory absolute)
					 absolute))
				     () nil t)))
	   (cond ((eq (car children) t) ;one marked item
		  (pop children))
		 ((cdr children)) ;multiple marked items
		 ((car children) ;no marked item, current line
		  (setq implicit-child t))))
	  (buffer-file-name
	   (let ((path (expand-file-name buffer-file-name)))
	     (setq parent (file-name-directory path)
		   children (list path))))
	  (default-directory
	   (setq parent (file-name-as-directory (expand-file-name default-directory))))
	  (t
	   (error "Can not determine any path")))
    ;; Check for Subversion working copy.
    (setq root (vc-svn-root parent))
    (when (null root)
      (error "The directory ‘%s’ is not within a Subversion working copy" parent))
    (setq root (file-name-as-directory (expand-file-name root)))
    ;; TODO: Is this reasonable?  If so, shall the check be performed
    ;; for all children?
    (let ((backend (vc-responsible-backend parent)))
      (unless (eq backend 'SVN)
	(error "The directory ‘%s’ is part of a %s repository" parent backend)))
    ;; Silently remove all children outside the working copy.
    (let ((admin (file-name-as-directory (expand-file-name vc-svn-admin-directory root))))
      (setq children (cl-delete-if (lambda (child)
				     (or (hare--file-name-lessp child root)
					 (string-prefix-p admin child hare--file-name-ignore-case)))
				   children)))
    ;; Sort children in ascending order.
    (setq children (sort (cl-delete-duplicates
			  children :test #'hare--file-name-equal)
			 #'hare--file-name-lessp))
    (when-let ((dir (and children (file-name-directory (car children)))))
      (when (hare--file-name-lessp dir parent)
	(setq parent dir)))
    ;; If PARENT is not under version control, adjust the paths.
    (let ((path parent))
      (while (and (hare--file-name-lessp root parent)
		  (memq (vc-state-refresh parent 'SVN) '(ignored unregistered nil)))
	(setq parent (file-name-directory (directory-file-name parent))))
      (when (and (hare--file-name-lessp parent path)
		 (null children))
	(setq children (list path))))
    ;; If there are no children, operate on PARENT.
    (when (null children)
      (setq children (list parent)))
    (when (and (null (cdr children))
	       (plist-get options :directory-files)
	       (hare--file-name-equal parent (car children)))
      (setcdr children (sort (delq nil (mapcar (lambda (relative)
						 (unless (hare--file-name-equal relative vc-svn-admin-directory)
      						   (let ((absolute (expand-file-name relative parent)))
						     (if (and (not (file-symlink-p absolute)) (file-directory-p absolute))
							 (file-name-as-directory absolute)
						       absolute))))
					       (directory-files parent nil directory-files-no-dot-files-regexp t)))
			     #'hare--file-name-lessp)))
    ;; Apply filters.
    (let ((filter (plist-get options :vc-state)))
      (cond ((eq filter t)
	     ;; Augment the paths with the VC state.
	     (setq children (mapcar (lambda (child)
				      (cons child (vc-state-refresh child 'SVN)))
				    children)))
	    ((consp filter)
	     (let (list state)
	       (if (not (eq (car filter) 'not))
		   (dolist (child children)
		     (setq state (vc-state-refresh child 'SVN))
		     (when (memq state filter)
		       (push (cons child state) list)))
		 ;; Drop ‘not’.
		 (setq filter (cdr filter))
		 (dolist (child children)
		   (setq state (vc-state-refresh child 'SVN))
		   (when (not (memq state filter))
		     (push (cons child state) list))))
	       (setq children (nreverse list))))))
    ;; Return values.
    (list root parent children)))

(defcustom hare-svn-interactive 'undefined
  "Whether or not to run ‘svn’ commands in an interactive shell."
  :type '(choice (const undefined) boolean)
  :group 'hare)

(defun hare--svn (buffer success targets command &rest options)
  "Execute a ‘svn’ command.
Return true if the command succeeds."
  (unless (listp vc-svn-global-switches)
    (error "User option ‘vc-svn-global-switches’ is not a list, please fix it"))
  (let ((shellp (if (eq hare-svn-interactive 'undefined)
		    (setq hare-svn-interactive (not (member "--non-interactive" vc-svn-global-switches)))
		  hare-svn-interactive))
	(status nil))
    (save-selected-window
      (with-current-buffer buffer
	(if (not shellp)
	    (let ((inhibit-read-only t)
		  (arguments (append vc-svn-global-switches
				     (when command (list command))
				     options
				     (cond ((listp targets)
					    (mapcar #'expand-file-name targets))
					   ((stringp targets)
					    (list (expand-file-name targets)))))))
	      ;; Show the command line.
	      (unless (bobp)
		(insert ?\n))
	      (insert vc-svn-program)
	      (dolist (argument arguments)
		(insert ?\s argument))
	      (insert ?\n)
	      (setq status (ignore-errors
			     (apply #'call-process vc-svn-program nil (list buffer t) t arguments)))
	      (unless (bolp)
		(insert ?\n))
	      (cond ((null status)
		     (insert (propertize "Failure:" 'face 'error)
			     " internal error"))
		    ((> status success)
		     (insert (propertize "Failure:" 'face 'error)
			     (format " exit status %s" status))
		     ;; Clear return value.
		     (setq status nil))
		    (t
		     (insert (propertize "Success:" 'face 'success)
			     (format " exit status %s" status))))
	      (insert ?\n))
	  ;; Interactive shell.
	  (unless (derived-mode-p 'shell-mode)
	    (setq buffer-read-only nil)
	    (shell buffer))
	  ;; Wait for the shell prompt and leave point after it.
	  (let* ((process (get-buffer-process buffer))
		 (last-output (process-mark process)))
	    (save-excursion
	      (goto-char last-output)
	      ;; This is like ‘beginning-of-line’ but ignores
	      ;; any text motion restrictions.
	      (forward-line 0)
	      (unless (looking-at shell-prompt-pattern)
		(accept-process-output process)))
	    ;; Leave point after the shell prompt.
	    (goto-char last-output))
	  ;; Insert the Subversion command.
	  (insert (comint-quote-filename vc-svn-program))
	  ;; Global options.
	  (dolist (option vc-svn-global-switches)
	    (insert ?\s option))
	  ;; The ‘svn’ sub-command.
	  (when command
	    (insert ?\s command))
	  ;; Sub-command options.
	  (dolist (option options)
	    (insert ?\s option))
	  ;; Targets.
	  (cond ((listp targets)
		 (dolist (target targets)
		   (insert ?\s (comint-quote-filename target))))
		((stringp targets)
		 (insert ?\s (comint-quote-filename targets))))
	  ;; Run it.
	  (comint-send-input nil t))
	()))
    ;; Return value.
    status))

(defun hare--svn-update (targets &rest options)
  "Run the ‘svn udpate’ command."
  (hare--with-process-window (buffer '(svn-update))
    (apply #'hare--svn buffer 0 targets "update"
	   (nconc (when-let ((revision (plist-get options :revision)))
		    (list "--revision" (hare--string revision)))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  (when-let ((set-depth (plist-get options :set-depth)))
		    (list "--set-depth" (hare--string set-depth)))
		  (when-let ((accept (plist-get options :accept)))
		    (list "--accept" (hare--string accept)))
		  (when (plist-get options :force)
		    (list "--force"))
		  (when (plist-get options :parents)
		    (list "--parents"))
		  (when (plist-get options :ignore-externals)
		    (list "--ignore-externals"))))))

(defun hare-svn-update (&optional arg)
  "Update your working copy."
  (interactive "P")
  (cl-multiple-value-bind (root parent children)
      (cl-values-list (hare--svn-collect-paths))
    (if (null children)
	(message "Nothing to do")
      (hare--form (targets revision depth set-depth accept force parents externals)
	  "Update your working copy.

Synchronize the working copy to the given revision.  If no revision is
specified, synchronize the working copy to the latest revision in the
repository."
	  (hare--svn-update targets
			    :revision revision
			    :depth depth
			    :set-depth set-depth
			    :accept accept
			    :force force
			    :parents parents
			    :ignore-externals externals)
	(setq revision (hare--form-svn-widget 'revision))
	(insert ?\n)
	(setq depth (hare--form-svn-widget 'depth))
	(insert ?\n)
	(setq set-depth (hare--form-svn-widget 'set-depth))
	(insert ?\n)
	(setq accept (hare--form-svn-widget 'accept))
	(insert ?\n)
	(setq force (hare--form-svn-widget 'checkbox
		      :doc "Handle unversioned obstructions as changes.
If enabled, unversioned paths in the working copy do not automatically
cause a failure if the update attempts to add the same path.  If the
unversioned path is the same type (file or directory) as the path in
the repository, it becomes versioned but its content is left as-is in
the working copy.  This means that an obstructing directory's unversioned
children may also obstruct and become versioned.  For files, any content
differences between the obstruction and the repository are treated like
a local modification to the working copy.  All properties from the
repository are applied to the obstructing path."))
	(insert ?\n)
	(setq parents (hare--form-svn-widget 'checkbox
			:doc "Create intermediate directories.
If a target is missing in the working copy but its immediate parent
directory is present, checkout the target into its parent directory
at the specified depth.  If this option is enabled, create any missing
parent directories of the target by checking them out at depth ‘empty’,
too."))
	(insert ?\n)
	(setq externals (hare--form-svn-widget 'checkbox
			  :doc "Ignore external definitions."))
	(hare--form-horizontal-line)
	(setq targets (hare--form-check-list children t))
	()))))

(defun hare--svn-cleanup (targets &rest options)
  "Run the ‘svn cleanup’ command."
  (let ((remove-unversioned
	 (when (plist-get options :remove-unversioned)
	   "--remove-unversioned"))
	(remove-ignored
	 (when (plist-get options :remove-ignored)
	   "--remove-ignored"))
	(vacuum-pristines
	 (when (plist-get options :vacuum-pristines)
	   "--vacuum-pristines"))
	(include-externals
	 (when (plist-get options :include-externals)
	   "--include-externals")))
    (hare--with-process-window (buffer '(svn-cleanup))
      (hare--all-null
       ;; The actual clean up command.
       (when (plist-get options :cleanup)
	 (not (apply #'hare--svn buffer 0 targets "cleanup"
		     (delq nil (list include-externals)))))
       ;; The alternative clean up command.
       (when (or remove-unversioned remove-ignored vacuum-pristines)
	 (not (apply #'hare--svn buffer 0 targets "cleanup"
		     (delq nil (list remove-unversioned
				     remove-ignored
				     vacuum-pristines
				     include-externals)))))))
    ()))

(defun hare--svn-add (targets &rest options)
  "Run the ‘svn add’ command."
  (hare--with-process-window (buffer '(svn-add))
    (apply #'hare--svn buffer 0 targets "add"
	   (nconc (when-let ((targets (plist-get options :targets)))
		    (list "--targets" (hare--string targets)))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  (when (plist-get options :no-ignore)
		    (list "--no-ignore"))
		  (when (plist-get options :auto-props)
		    (list "--auto-props"))
		  (when (plist-get options :no-auto-props)
		    (list "--no-auto-props"))
		  (when (plist-get options :force)
		    (list "--force"))
		  (when (plist-get options :parents)
		    (list "--parents"))))))

(defun hare-svn-add (&optional arg)
  "Put files and directories under version control."
  (interactive "P")
  (cl-multiple-value-bind (root parent children)
      (cl-values-list (hare--svn-collect-paths
		       :vc-state '(unregistered)
		       :directory-files t))
    (if (null children)
	(message "Nothing to do")
      (hare--form (targets depth force no-ignore auto-props parents)
	  "Put files and directories under version control."
	  (hare--svn-add targets
			 :depth depth
			 :no-ignore no-ignore
			 :auto-props (eq auto-props t)
			 :no-auto-props (eq auto-props nil)
			 :force force
			 :parents parents)
	(setq depth (hare--form-svn-widget 'depth
		      :value 'empty))
	(insert ?\n)
	(setq no-ignore (hare--form-svn-widget 'checkbox
			  :doc "Don't apply ignore rules to implicitly added items.
Subversion uses ignore patterns to determine which items should be
skipped as part of a larger recursive operation.  If this option is
enabled, operate on all the files and directories present."))
	(insert ?\n)
	(setq auto-props (hare--form-svn-widget 'auto-props))
	(insert ?\n)
	(setq force (hare--form-svn-widget 'checkbox
		      :value t
		      :doc "Ignore already versioned paths.
If this option is enabled, add all the unversioned paths and ignore
the rest.  Otherwise, error out if a path is already versioned."))
	(insert ?\n)
	(setq parents (hare--form-svn-widget 'checkbox
			:value t
			:doc "Add intermediate directories.
If this option is enabled, add any missing parent directories of the
target at depth ‘empty’, too."))
	(hare--form-horizontal-line)
	(setq targets (hare--form-check-list (mapcar #'car children) t))
	()))))

(defun hare-svn-cleanup (&optional arg)
  "Recursively clean up the working copy."
  (interactive "P")
  (cl-multiple-value-bind (root)
      (cl-values-list (hare--svn-collect-paths))
    (let ((default-directory root))
      (hare--form (cleanup unversioned ignored vacuum externals)
	  "Recursively clean up the working copy."
	  (hare--svn-cleanup default-directory ;see ‘hare--form-special’
			     :cleanup cleanup
			     :remove-unversioned unversioned
			     :remove-ignored ignored
			     :vacuum-pristines vacuum
			     :include-externals externals)
	(setq cleanup (hare--form-svn-widget 'checkbox
			:doc "Clean up working copy status.
Remove all write locks (shown as ‘L’ by the ‘svn status’ command) from
the working copy.  Usually, this is only necessary if a Subversion client
has crashed while using the working copy, leaving it in an unusable state."
			:value t))
	(insert ?\n)
	(setq unversioned (hare--form-svn-widget 'checkbox
			    :doc "Remove unversioned files and directories."))
	(setq ignored (hare--form-svn-widget 'checkbox
			:doc "Remove ignored files and directories."))
	(setq vacuum (hare--form-svn-widget 'checkbox
		       :doc "Remove unreferenced original files from ‘.svn’ directory."))
	(insert ?\n)
	(setq externals (hare--form-svn-widget 'checkbox
			  :doc "Include external definitions.
Also operate on externals defined by ‘svn:externals’ properties."))
	()))))

(defconst hare--svn-menu
  (let ((menu (make-sparse-keymap "HareSVN")))
    (bindings--define-key menu [hare-svn-cleanup]
      '(menu-item "Clean up..." hare-svn-cleanup
		  :help "Recursively clean up the working copy"))
    (bindings--define-key menu [hare-svn-add]
      '(menu-item "Add..." hare-svn-add
		  :help "Put files and directories under version control"))
    (bindings--define-key menu [hare-svn-update]
      '(menu-item "Update..." hare-svn-update
		  :help "Update your working copy"))
    menu)
  "HareSVN menu for Subversion.")

(provide 'hare)

;;; hare.el ends here

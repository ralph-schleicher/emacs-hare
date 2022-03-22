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
	  (insert " ")
	  (let ((mark (point)))
	    (insert (cl-getf prop :flag))
	    (put-text-property mark (point) 'help-echo (cl-getf prop :help)))
	  (when (display-graphic-p)
	    (insert " ")
	    (hare--insert-image icon nil `(help-echo ,(cl-getf prop :help))))
	  (insert " " (symbol-name state) "\n")))
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
	 "Version control system: " (format "%s" vc-dir-backend) "\n"
	 ;; The VC working directory information is redundant since
	 ;; it is equal to the top-level Dired directory header.
	 "Working directory: " (abbreviate-file-name default-directory) "\n"
	 (if-let ((headers (vc-call-backend vc-dir-backend
					    'dir-extra-headers
					    default-directory)))
	     (concat (replace-regexp-in-string " +:" ":" headers) "\n")
	   "")
	 "\n")
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
	    (insert " ")))
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

(defun hare--temp-buffer-window ()
  "Create a temporary HareSVN buffer and show it in a window.
Does not change the selected window or the current buffer.

Return value is the window displaying the buffer.  The buffer
itself is empty."
  (let ((came-from (current-buffer)))
    (with-current-buffer-window
	hare--temp-buffer-name hare--temp-buffer-action
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
	 (widget-insert hare--form-horizontal-line "\n"))
	((characterp hare--form-horizontal-line)
	 (widget-insert (make-string (- (window-text-width) 2) hare--form-horizontal-line) "\n"))))

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
	    (,window (hare--temp-buffer-window))
	    (,buffer (window-buffer ,window)))
       (select-window ,window)
       (set-buffer ,buffer)
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
       ;; Insert the documentation string.
       (widget-insert ,documentation "\n" "\n")
       ;; Insert the cancel and submit buttons.  There is an empty
       ;; line before and after these buttons.
       (widget-insert " ")
       (let* ((button (hare--form-quit-button " Cancel "))
	      (fire (lambda ()
		      (interactive)
		      (widget-item-action button))))
	 (local-set-key (kbd "C-c C-q") fire)
	 (local-set-key (kbd "C-c q") fire))
       (widget-insert " " " ")
       (let* ((button (hare--form-quit-button "   OK   "
			,@(when submit-form (list submit-form))))
	      (fire (lambda ()
		      (interactive)
		      (widget-item-action button))))
	 (local-set-key (kbd "C-c C-c") fire)
	 (local-set-key (kbd "C-c c") fire))
       (widget-insert "\n" "\n")
       (hare--form-horizontal-line)
       ;; The body form.
       ,@body
       ;; Prepare text entries.
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

(defmacro hare--form-check-list (list-of-strings checked)
  "Insert a check list into the form.

First argument LIST-OF-STRINGS are the check list items.
Second argument CHECKED determines the initial state of
 the check list items."
  (let ((widget (gensym "widget")))
    `(let (,widget)
       (unless (bolp)
         (widget-insert "\n"))
       (widget-insert "Check: ")
       (widget-create 'push-button
                      :notify (lambda (&rest _ignore)
                                (dolist (button (widget-get ,widget :buttons))
                                  (unless (widget-value button)
                                    (widget-checkbox-action button))))
                      " All ")
       (widget-insert " ")
       (widget-create 'push-button
                      :notify (lambda (&rest _ignore)
                                (dolist (button (widget-get ,widget :buttons))
                                  (when (widget-value button)
                                    (widget-checkbox-action button))))
                      " None ")
       (widget-insert "\n")
       (widget-insert "\n")
       (setq ,widget (apply #'widget-create 'checklist
			    :entry-format " %b %v"
                            (mapcar (lambda (string)
                                      `(item ,string))
                                    ,list-of-strings)))
       (hare--form-horizontal-line)
       (let ((flag (not (null ,checked))))
	 (dolist (button (widget-get ,widget :buttons))
           (unless (eq (widget-value button) flag)
             (widget-checkbox-action button))))
       ,widget)))

;;;; Subversion

(defun hare--svn-update (files)
  "Run the ‘svn udpate’ command."
  (let ((buffer (window-buffer (hare--temp-buffer-window))))
    (vc-svn-command buffer 0 files "update")))

(defun hare-svn-update ()
  "Update your working copy."
  (interactive)
  ;; TODO: Consider calling ‘(vc-deduce-fileset t)’.
  (let ((files (cond ((derived-mode-p 'dired-mode)
		      (or (sort (delq nil (dired-map-over-marks
					   (dired-get-filename nil t)
					   nil))
				#'string<)
			  (list default-directory)))
		     (t
		      (when-let ((file (or buffer-file-name
					   default-directory)))
			(list file))))))
    (if (null files)
	(message "Nothing to do")
      (hare--form (checked-files)
	  "Update your working copy."
	  (hare--svn-update checked-files)
	(setq checked-files (hare--form-check-list files t))
	()))))

(defun hare--svn-cleanup (working-directory &rest options)
  "Run the ‘svn cleanup’ command."
  ())

(defun hare-svn-cleanup ()
  "Recursively clean up the working copy."
  (interactive)
  (if (null default-directory)
      (message "Nothing to do")
    (hare--form (cleanup unlock refresh externals unversioned ignored revert)
	"Recursively clean up the working copy."
	(hare--svn-cleanup default-directory ;see ‘hare--form-special’
			   :cleanup cleanup
			   :break-locks unlock
			   :refresh-icons refresh
			   :include-externals externals
			   :delete-unversioned unversioned
			   :delete-ignored ignored
			   :revert revert)
      (setq cleanup (widget-create 'checkbox
				   :format " %[%v%] %t"
				   :tag "Clean up working copy status"
				   t))
      (widget-insert "\n")
      (setq unlock (widget-create 'checkbox
				  :format " %[%v%] %t"
				  :tag "Break write locks"
				  t))
      (widget-insert "\n")
      (setq refresh (widget-create 'checkbox
				   :format " %[%v%] %t"
				   :tag "Refresh HareSVN icons"
				   nil))
      (widget-insert "\n")
      (setq externals (widget-create 'checkbox
				     :format " %[%v%] %t"
				     :tag "Include externals"
				     t))
      (widget-insert "\n")
      (setq unversioned (widget-create 'checkbox
				       :format " %[%v%] %t"
				       :tag "Delete unversioned files and directories"
				       nil))
      (widget-insert "\n")
      (setq ignored (widget-create 'checkbox
				   :format " %[%v%] %t"
				   :tag "Delete ignored files and directories"
				   nil))
      (widget-insert "\n")
      (setq revert (widget-create 'checkbox
				  :format " %[%v%] %t"
				  :tag "Revert all changes recursively"
				  nil))
      (widget-insert "\n")
      ())))

(defconst hare--svn-menu
  (let ((menu (make-sparse-keymap "HareSVN")))
    (bindings--define-key menu [hare-svn-cleanup]
      '(menu-item "Clean up..." hare-svn-cleanup
		  :help "Recursively clean up the working copy"))
    (bindings--define-key menu [hare-svn-update]
      '(menu-item "Update" hare-svn-update
		  :help "Update your working copy"))
    menu)
  "HareSVN menu for Subversion.")

(provide 'hare)

;;; hare.el ends here

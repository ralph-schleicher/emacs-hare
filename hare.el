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

;;; Commentary:

;; In Dired, enter ‘M-x hare-dired-mode RET’ to display the VC state
;; icon in front of a file name.  Clicking with the mouse on the VC
;; state icon pops up a menu for running VC commands.  If the Dired
;; buffer displays a part of a Subversion working copy, the menu is
;; Subversion specific.  Otherwise, the menu is equal to the generic
;; VC menu.

;;; Code:

(require 'cl-lib)
(require 'vc-svn)
(require 'vc-dir)
(require 'dired)
(require 'subr-x)
(require 'log-edit)
(require 'wid-edit)
(require 'cus-edit)
(require 'shell)
(require 'diff-mode)

(defgroup hare nil
  "HareSVN is a TortoiseSVN clone for Dired buffers."
  :prefix "hare-"
  :group 'vc
  :group 'dired)

(defun hare--vc-state (file backend)
  "Return the HareSVN VC state of FILE for BACKEND.

Value is the VC state symbol as returned by the ‘vc-state’ function,
or ‘locked’ if the VC state indicates that the file is locked by some
other user."
  (let ((state (vc-state file backend)))
    (if (stringp state) 'locked state)))

(defun hare--vc-state-refresh (file backend)
  "Return the updated HareSVN VC state of FILE for BACKEND.

Value is the VC state symbol as returned by the ‘vc-state’ function,
or ‘locked’ if the VC state indicates that the file is locked by some
other user."
  (let ((state (vc-state-refresh file backend)))
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
  "Return non-nil if VC states are visualized with the help of graphic icons."
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

(defun hare--insert-icon (state &optional text-properties)
  "Insert an VC state icon into the current buffer at point.
First argument STATE is the VC state.
Optional second argument TEXT-PROPERTIES are additional text properties."
  (unless (memq state hare--vc-states)
    (error "Unknown VC state ‘%s’" state))
  (let ((prop (cdr (assq state hare--vc-state-alist)))
	(icon (when (hare--display-graphic-p)
		(cdr (assq state hare--icon-alist))))
	(mark (point)))
    (if (null icon)
	(insert (plist-get prop :flag))
      (insert "  "))
    (when-let ((help (plist-get prop :help)))
      (put-text-property mark (point) 'help-echo help))
    (when text-properties
      (add-text-properties mark (point) text-properties))
    (when icon
      (add-text-properties
       mark (point) `(display ,icon
		      rear-nonsticky t
		      cursor-intangible t)))
    ()))

;;;###autoload
(defun hare-list-vc-states ()
  "Display the HareSVN version control states in a help window."
  (interactive)
  (hare-update-icons)
  (with-help-window "*VC States*"
    (with-current-buffer standard-output
      (erase-buffer)
      (insert "List of HareSVN version control states.\n\n")
      (dolist (cell hare--vc-state-alist)
	(let ((state (car cell)))
	  (insert ?\s)
	  (let ((hare-display-icons nil))
	    (hare--insert-icon state))
	  (when (display-graphic-p)
	    (insert ?\s)
	    (let ((hare-display-icons t))
	      (hare--insert-icon state)))
	  (insert ?\s (symbol-name state) ?\n)))
      (setq cursor-type nil)
      (setq truncate-lines t))))

(defun hare-refresh-vc-states ()
  "Refresh the VC states and redisplay the current buffer."
  (interactive)
  (cond ((obarrayp vc-file-prop-obarray)
	 (mapatoms (lambda (symbol)
		     (let ((file (symbol-name symbol)))
		       (when-let ((backend (vc-backend file)))
			 (vc-state-refresh file backend))))
		   vc-file-prop-obarray)))
  (cond ((derived-mode-p 'dired-mode)
	 (revert-buffer)))
  ())

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
      		 (state (and file vc-dir-backend (hare--vc-state file vc-dir-backend))))
	    (hare--insert-icon state `(keymap ,hare--dired-icon-keymap))
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
  (revert-buffer))

;;;; Paths

(defun hare--string-equal (string1 string2 &optional ignore-case)
  "Return non-nil if STRING1 is equal to STRING2 in lexicographic order.
Case is not significant if optional argument IGNORE-CASE is non-nil."
  (let ((ans (compare-strings string1 0 nil string2 0 nil ignore-case)))
    (eq ans t)))

(defun hare--string-lessp (string1 string2 &optional ignore-case)
  "Return non-nil if STRING1 is less than STRING2 in lexicographic order.
Case is not significant if optional argument IGNORE-CASE is non-nil."
  (let ((ans (compare-strings string1 0 nil string2 0 nil ignore-case)))
    (and (numberp ans) (cl-minusp ans))))

(defvar hare--file-name-ignore-case (memq system-type '(ms-dos windows-nt))
  "Whether or not to ignore case when comparing file names.")

(defun hare--file-name-equal (name1 name2)
  "Return non-nil if NAME1 is equal to NAME2 in lexicographic order.
Whether or not case is significant depends on the current value of
the variable ‘hare--file-name-ignore-case’."
  (hare--string-equal name1 name2 hare--file-name-ignore-case))

(defun hare--file-name-lessp (name1 name2)
  "Return non-nil if NAME1 is less than NAME2 in lexicographic order.
Whether or not case is significant depends on the current value of
the variable ‘hare--file-name-ignore-case’."
  (hare--string-lessp name1 name2 hare--file-name-ignore-case))

(defun hare--expand-file-name (relative directory)
  "Like ‘expand-file-name’ but mark directories as such."
  (let ((absolute (expand-file-name relative directory)))
    (if (and (file-directory-p absolute) (not (file-symlink-p absolute)))
	(file-name-as-directory absolute)
      absolute)))

(defun hare--directory-files (directory)
  "Like ‘directory-files’ but ignore VC administration directories."
  (sort (delq nil (mapcar (lambda (relative)
			    (unless (cl-member relative vc-directory-exclusion-list
					       :test #'hare--file-name-equal)
			      (hare--expand-file-name relative directory)))
			  (directory-files directory nil directory-files-no-dot-files-regexp t)))
	#'hare--file-name-lessp))

(cl-defstruct (hare--path
	       (:constructor nil)
	       (:constructor hare--make-path)
	       (:copier nil)
	       (:type vector))
  "A path, i.e. file name object.

This type is only used as a child item of a HareSVN paths structure."
  (absolute nil
   :documentation "The absolute file name.")
  (relative nil
   :documentation "The relative file name.")
  (vc-state t ;not nil
   :documentation "The path's VC state."))

(defsubst hare--path-directory-p (path)
  "Return non-nil if PATH is a directory."
  (directory-name-p (hare--path-absolute path)))

(cl-defstruct (hare--paths
	       (:constructor nil)
	       (:constructor hare--make-paths)
	       (:copier hare--copy-paths))
  "A collection of paths, i.e. file names."
  (parent nil
   :documentation "The directory file name containing all children.")
  (children ()
   :documentation "The list of child items.")
  (insertions ()
   :documentation "The list of inserted directories.")
  (vc-root nil
   :documentation "The top-level working copy directory file name.")
  (vc-backend nil
   :documentation "The responsible VC backend.")
  (vc-state nil
   :documentation "The applicable VC state filter."))

(defun hare--path-from-file (parent children &optional vc-backend vc-state)
  "Destructively replace file names by corresponding path structures.

First argument PARENT is the directory file name containing all children.
Second argument CHILDREN is the list of child file names.
Optional third argument VC-BACKEND is the responsible VC backend.
Optional fourth argument VC-STATE is the applicable VC state filter.

Return value is the modified list of child items."
  ;; Replace file name by path structure.
  (let ((start (length parent)))
    (cl-mapl (lambda (cell)
	       (let ((child (car cell)))
		 (setcar cell (hare--make-path
			       :absolute child
			       :relative (let ((str (substring child start)))
					   (if (zerop (length str)) "." str))))))
	     children))
  ;; Handle VC state.
  (when (and vc-backend vc-state)
    ;; Augment the paths with the VC state.
    (dolist (path children)
      (setf (hare--path-vc-state path)
	    (hare--vc-state-refresh (hare--path-absolute path) vc-backend)))
    ;; Apply VC state filter.
    (when (consp vc-state)
      (setq children (if (eq (car vc-state) 'not)
			 (let ((vc-state (cdr vc-state)))
			   (cl-delete-if (lambda (path)
					   (memq (hare--path-vc-state path) vc-state))
					 children))
		       (cl-delete-if-not (lambda (path)
					   (memq (hare--path-vc-state path) vc-state))
					 children)))))
  ;; Return the modified list.
  children)

(defun hare--collect-paths (&rest options)
  "Collect paths, i.e. files and directories.

Start with the set of selected files and directories.  The current
working directory is called the parent and the selected items are
the children.  Only the children are part of the collection.

If keyword argument IGNORE-SELECTED is non-nil, ignore any
 selected item, i.e. start with an empty set.
If keyword argument IGNORE-MARKS is non-nil, ignore marks, i.e.
 just collect the current item.  This option only has an effect
 if IGNORE-SELECTED is nil and the current mode has the concept
 of marked items; like, for example, Dired.
If keyword argument REQUIRE-SELECTED is non-nil, signal an error
 if no item is selected.  This option is mutually exclusive to
 the IGNORE-SELECTED option.  Specifying both options will always
 signal an error.

After that the parent directory will be fixed.  If VC options
are enabled, the parent directory will be adjusted so that it
is a registered directory within the repository.  If the parent
moves upwards in the directory hierarchy and the collection is
empty, the current working directory, i.e. the initial parent,
becomes a child item.  Additionally, all child items outside
the repository are silently removed.  If no children remain,
collect the parent.

If keyword argument COLLECT-PARENT is non-nil, unconditionally
 add the parent to the collection.
If keyword argument PARENT-ITEMS is non-nil, collect the parent's
 immediate child items iff no other child items are selected.

Various VC options are assessed based on the current working
directory.

If keyword argument VC-BACKEND is non-nil, signal an error if no
 responsible VC backend can be found.  If VC-BACKEND is a list,
 signal an error is the responsible VC backend is not a member
 of the list.
If keyword argument VC-ROOT is non-nil, signal an error if the VC
 backend's top-level working directory can't be found.
If keyword argument VC-STATE is non-nil, determine the VC state of
 the children, too.  If VC-STATE is a list, only collect children
 who's VC state is a member of the list.  If the list starts with
 ‘not’, complement the test.

Return value is a HareSVN paths structure.  If there are no children,
signal an error unless keyword argument NO-CHILDREN is non-nil."
  (let (parent children root backend)
    ;; Collect absolute path names.  Expand file name abbreviations,
    ;; like ‘~/foo’, and mark directory file names as directories.
    (cond ((derived-mode-p 'dired-mode)
	   (setq parent (file-name-as-directory (expand-file-name default-directory))
		 children (cond ((plist-get options :ignore-selected)
				 ())
				((plist-get options :ignore-marks)
				 (when-let ((relative (dired-get-filename t t)))
				   (list (hare--expand-file-name relative parent))))
				((delq nil (dired-map-over-marks
					    ;; Expand ‘.’ and ‘..’ and mark directory
					    ;; file names as directories.
					    (when-let ((relative (dired-get-filename t t)))
					      (hare--expand-file-name relative parent))
					    ()))))))
	  (buffer-file-name
	   (let ((path (expand-file-name buffer-file-name)))
	     (setq parent (file-name-directory path)
		   children (if (plist-get options :ignore-selected) () (list path)))))
	  (default-directory
	   (setq parent (file-name-as-directory (expand-file-name default-directory))))
	  (t
	   (error "Can not determine any path")))
    (when (and (null children) (plist-get options :require-selected))
      (error "No path selected"))
    ;; Handle the VC options.
    (let ((vc-backend (plist-get options :vc-backend))
	  (vc-root (plist-get options :vc-root))
	  (vc-state (plist-get options :vc-state)))
      (when (or vc-backend vc-root vc-state)
	;; Determine the VC backend.
	(setq backend (vc-responsible-backend parent))
	(when (null backend)
	  (error "The directory ‘%s’ is not part of a repository" parent))
	(when (and vc-backend (not (eq vc-backend t)))
	  (unless (memq backend (if (consp vc-backend) vc-backend (list vc-backend)))
	    (error "The directory ‘%s’ is part of a %s repository" parent backend)))
	;; Determine the VC root.
	(setq root (condition-case nil
		       (vc-call-backend backend 'root parent)
		     (vc-not-supported)))
	(when (and vc-root (null root))
	  (error "The directory ‘%s’ is not part of a %s repository" parent backend))))
    ;; Silently remove all children outside the repository.
    (setq root (if (null root)
		   parent
		 (file-name-as-directory (expand-file-name root))))
    (setq children (cl-delete-if (lambda (child)
				   (hare--file-name-lessp child root))
				 children))
    (when (not (null backend))
      (when-let ((admin (condition-case nil
			    (vc-call-backend backend 'find-admin-dir root)
			  (vc-not-supported))))
	(setq admin (file-name-as-directory (expand-file-name admin)))
	(setq children (cl-delete-if (lambda (child)
				       (string-prefix-p admin child hare--file-name-ignore-case))
				     children))))
    ;; Sort children in ascending order.
    (setq children (sort (cl-delete-duplicates
			  children :test #'hare--file-name-equal)
			 #'hare--file-name-lessp))
    ;; Ensure that the parent contains all children.
    (when-let ((dir (and children (file-name-directory (car children)))))
      (when (hare--file-name-lessp dir parent)
	(setq parent dir)))
    ;; If the parent is not under version control, adjust the paths.
    (when (not (null backend))
      (let ((path parent))
	(while (and (hare--file-name-lessp root parent)
		    (memq (hare--vc-state-refresh parent backend) '(ignored unregistered nil)))
	  (setq parent (file-name-directory (directory-file-name parent))))
	(when (and (hare--file-name-lessp parent path)
		   (null children))
	  (setq children (list path)))))
    ;; Create the HareSVN paths structure.
    (let ((paths (let ((state (plist-get options :vc-state)))
		   (when (consp state)
		     (setq state (copy-sequence state)))
		   (hare--make-paths
		    :parent parent
		    :children (hare--path-from-file parent children backend state)
		    :vc-root root
		    :vc-backend backend
		    :vc-state state))))
      ;; If there are no children, operate on the parent.
      (when (or (null (hare--paths-children paths))
		(and (plist-get options :collect-parent)
		     (not (hare--file-name-equal
			   (hare--path-absolute (car (hare--paths-children paths)))
			   parent))))
	(setf (hare--paths-children paths)
	      (nconc (hare--path-from-file
		      (hare--paths-parent paths)
		      (list (hare--paths-parent paths))
		      (hare--paths-vc-backend paths)
		      (not (null (hare--paths-vc-state paths))))
		     (hare--paths-children paths))))
      ;; When operating on the parent only, optionally include the
      ;; parent's files and directories.
      (when (and (plist-get options :parent-items)
		 (hare--file-name-equal
		  (hare--path-absolute (car (hare--paths-children paths)))
		  parent)
		 (null (cdr (hare--paths-children paths))))
	(hare--paths-insert-subdir paths (car (hare--paths-children paths))))
      ;; Final check.
      (unless (or (hare--paths-children paths) (plist-get options :no-children))
	(error "No path"))
      paths)))

(defun hare--paths-insert-subdir (paths subdir &optional _recursively)
  "Insert the files and directories of a subdirectory.

First argument PATHS is a HareSVN paths structure.
Second argument SUBDIR is a path structure.  Signal an error if SUBDIR is
 not an existing child of PATHS.  Do nothing if SUBDIR is not a directory.
Optional third argument RECURSIVELY has no effect.

Return value is non-nil if the HareSVN paths structure has been modified."
  (let ((here (memq subdir (hare--paths-children paths))))
    (when (null here)
      (error "Not a child"))
    (when-let ((items (when (and (hare--path-directory-p subdir)
				 (not (cl-member (hare--path-absolute subdir)
						 (hare--paths-insertions paths)
						 :test #'hare--file-name-equal)))
			(hare--path-from-file
			 (hare--paths-parent paths)
			 (hare--directory-files (hare--path-absolute subdir))
			 (hare--paths-vc-backend paths)
			 (hare--paths-vc-state paths)))))
      ;; Insert ITEMS after HERE into the list of children.
      (let ((tail (cdr here)))
	(setcdr here (nconc items tail)))
      ;; Mark subdirectory as inserted.
      (push (hare--path-absolute subdir) (hare--paths-insertions paths))
      t)))

(defun hare--paths-remove-subdir (paths subdir)
  "Remove the files and directories of a subdirectory.

First argument PATHS is a HareSVN paths structure.
Second argument SUBDIR is a path structure.  Signal an error if SUBDIR is
 not an existing child of PATHS.  Do nothing if SUBDIR is not a directory.

Return value is non-nil if the HareSVN paths structure has been modified."
  (let ((here (memq subdir (hare--paths-children paths))))
    (when (null here)
      (error "Not a child"))
    (when (hare--path-directory-p subdir)
      (let (modified)
	(setf (hare--paths-children paths)
	      (cl-delete-if (lambda (child)
			      (when (and (not (eq subdir child))
					 (string-prefix-p
					  (hare--path-absolute subdir)
					  (hare--path-absolute child)
					  hare--file-name-ignore-case))
				(setq modified t)))
			    (hare--paths-children paths)))
	;; Clear insertion mark.
	(setf (hare--paths-insertions paths)
	      (cl-delete-if (lambda (dir)
			      (string-prefix-p
			       (hare--path-absolute subdir) dir
			       hare--file-name-ignore-case))
			    (hare--paths-insertions paths)))
	modified))))

;;;; Log Messages

(defun hare--trim-log-message (&optional final-newline)
  "Delete superfluous whitespace in a log message buffer.

If optional argument FINAL-NEWLINE is nil, delete all final newline
 characters.  Otherwise, preserve one explicit final newline character.
 If FINAL-NEWLINE is the symbol ‘log-edit-require-final-newline’, apply
 the same semantics as documented for this user option.

This function does not preserve point."
  ;; Delete trailing whitespace.
  (goto-char (point-min))
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match "" t t))
  ;; Delete leading empty lines.
  (goto-char (point-min))
  (skip-chars-forward "\n")
  (unless (bobp)
    (delete-region (point-min) (point)))
  ;; Delete trailing empty lines.
  (goto-char (point-max))
  (skip-chars-backward "\n")
  (when (and (not (eobp)) final-newline)
    (forward-char 1)) ;preserve final newline
  (unless (eobp)
    (delete-region (point) (point-max)))
  ;; Check for final newline.
  (when (eq final-newline 'log-edit-require-final-newline)
    (setq final-newline log-edit-require-final-newline))
  (when (and (> (point-max) (point-min))
	     (/= (char-before (point-max)) ?\n)
	     (or (eq final-newline t)
		 (and final-newline
		      (y-or-n-p
		       "Log message does not end in newline.  Add one? "))))
    (goto-char (point-max))
    (insert ?\n)))

(cl-defmacro hare--with-log-message-file ((file-var message) &body body)
  "Write a log message to a temporary file, then evaluate BODY.

If first argument FILE-VAR is non-nil, bind the temporary file name to
 this variable so that BODY can use it.
Second argument MESSAGE is the log message."
  (declare (indent 1))
  (let ((file (gensym "file")))
    `(let* ((,file (make-temp-file "log"))
 	    ,@(when file-var `((,file-var ,file))))
       (unwind-protect
	   (progn
	     (with-temp-file ,file
	       (insert (or ,message ""))
	       (hare--trim-log-message 'log-edit-require-final-newline)
	       (log-edit-remember-comment))
	     ,@body)
	 (ignore-errors
	   (delete-file ,file))))))

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

Optional arguments BUFFER-NAME and BUFFER-ACTION override
 ‘hare--temp-buffer-name’ and ‘hare--temp-buffer-action’
 respectively.

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

(defvar hare--form-special '(default-directory)
  "List of special variables.
See the ‘hare--form’ macro for more details.")

(defvar-local hare--form-special-symbols ()
  "List of ‘hare--form-special’ symbols.")

(defvar-local hare--form-special-values ()
  "List of ‘hare--form-special’ symbol values.")

(defvar-local hare--form-widget-names ()
  "List of widget names, i.e. symbols binding a widget.
See the ‘hare--form’ macro for more details.")

(defvar-local hare--form-cancel-button nil
  "The cancel push button.")

(defvar-local hare--form-submit-button nil
  "The submit push button.")

(defun hare-cancel-form ()
  "Cancel the HareSVN form."
  (interactive)
  (when (widgetp hare--form-cancel-button)
    (widget-item-action hare--form-cancel-button)))

(defun hare-submit-form ()
  "Submit the HareSVN form."
  (interactive)
  (when (widgetp hare--form-submit-button)
    (widget-item-action hare--form-submit-button)))

(defvar hare-form-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-q") 'hare-cancel-form)
    (define-key map (kbd "C-c q") 'hare-cancel-form)
    (define-key map (kbd "C-c C-c") 'hare-submit-form)
    (define-key map (kbd "C-c c") 'hare-submit-form)
    map)
  "Keymap for HareSVN form buffers.
The parent of this keymap is ‘widget-keymap’ so that the user can tab
through the widgets of the form.")

;;;###autoload
(define-derived-mode hare-form-mode special-mode "HareSVN-Form"
  "Major mode for HareSVN form buffers."
  :group 'hare
  :syntax-table nil
  :abbrev-table nil
  :interactive nil
  ;; Use the widget style of the customization engine.
  (custom--initialize-widget-variables))

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

   * The documentation string is inserted at the beginning of the form
     buffer.

   * A cancel and submit push button is inserted.  Both buttons destroy
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
       ;; Set major mode.
       (hare-form-mode)
       ;; Forms are editable.
       (setq buffer-read-only nil)
       ;; Propagate the special variables.
       (setq hare--form-special-symbols (copy-sequence hare--form-special))
       (setq hare--form-special-values ,values)
       (cl-mapc (lambda (symbol value)
		  (set (make-local-variable symbol) value))
		hare--form-special-symbols hare--form-special-values)
       ;; Bind the variables for the widget names.
       (setq hare--form-widget-names ',widget-names)
       (dolist (widget-name hare--form-widget-names)
	 (set (make-local-variable widget-name) nil))
       ;; Prepare the form.
       (let ((inhibit-modification-hooks t))
	 ;; Insert the documentation string.
	 (insert ,documentation ?\n ?\n)
	 ;; Insert the cancel and submit buttons.  There is an empty
	 ;; line before and after these buttons.
	 (insert ?\s)
	 (setq hare--form-cancel-button (hare--form-quit-button " Cancel "))
	 (insert ?\s ?\s)
	 (setq hare--form-submit-button (hare--form-quit-button "   OK   "
       					  ,@(when submit-form (list submit-form))))
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

(defun hare--widget-value (widget)
  "Like ‘widget-value’ but return nil if WIDGET is not a widget."
  (when (widgetp widget)
    (widget-value widget)))

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

(define-widget 'hare--path-widget 'const
  "A file name with optional VC state.
Value is a ‘hare--path’ structure."
  :format "%v"
  :value-create 'hare--path-widget-value-create)

(defvar hare--path-widget-directory-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map hare-form-mode-map)
    (define-key map "\r" 'hare--paths-widget-insert-subdir) ;RET
    (define-key map "\d" 'hare--paths-widget-remove-subdir) ;DEL
    (define-key map [mouse-2] 'hare--paths-widget-insert-subdir-event)
    (define-key map [mouse-3] 'hare--paths-widget-remove-subdir-event)
    (define-key map [down-mouse-2] 'ignore)
    (define-key map [down-mouse-3] 'ignore)
    map)
  "Keymap for a directory path.")

(defun hare--path-widget-value-create (widget)
  "Insert the printed representation of the value."
  (let ((path (widget-get widget :value)))
    (let ((state (hare--path-vc-state path)))
      (unless (eq state t)
	(hare--insert-icon state)
	(insert ?\s)))
    (let ((absolute (hare--path-absolute path))
	  (relative (hare--path-relative path))
	  (mark (point)))
      (if (null relative)
	  (insert absolute)
	(let ((mark (point)))
	  (insert relative)
	  (put-text-property mark (point) 'help-echo absolute)))
      (put-text-property mark (point) 'hare-path widget)
      (when (hare--path-directory-p path)
	(let ((help (concat (when-let ((help (get-text-property mark 'help-echo)))
			      (concat help "\n\n"))
			    "mouse-2: Insert directory contents" "\n"
			    "mouse-3: Remove directory contents")))
	  (put-text-property mark (point) 'help-echo help))
	(put-text-property mark (point) 'keymap hare--path-widget-directory-keymap)))))

(defun hare--path-widget-at-point ()
  "Return the path widget at point."
  (save-excursion
    (forward-line 0)
    (when-let* ((limit (line-end-position))
		(start (next-single-property-change
			(point) 'hare-path nil limit))
		(end (next-single-property-change
		      start 'hare-path nil limit)))
      (get-text-property start 'hare-path))))

(defvar-local hare--paths-widget nil
  "The paths widget for selecting file names.")

(define-widget 'hare--paths-widget 'default
  "A widget for selecting multiple file names.
The user can select/deselect items interactively.

Value is a HareSVN paths structure."
  :format "%v"
  :value-create 'hare--paths-widget-value-create
  :value-get 'hare--paths-widget-value-get
  ;; Embedded widgets.
  :hare-set-operation nil
  :hare-checklist nil
  ;; Options.
  :hare-checked t)

(defun hare--paths-widget-value-create (widget)
  "Insert the printed representation of the value."
  (let ((paths (widget-get widget :value)))
    (unless (hare--paths-p paths)
      (error "Invalid value"))
    (setq hare--paths-widget widget)
    (insert "Check: ")
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
			     (let ((checklist (widget-get hare--paths-widget :hare-checklist)))
                               (dolist (button (widget-get checklist :buttons))
				 (unless (widget-value button)
                                   (widget-checkbox-action button)))))
		   " All ")
    (insert ?\s)
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
			     (let ((checklist (widget-get hare--paths-widget :hare-checklist)))
                               (dolist (button (widget-get checklist :buttons))
				 (when (widget-value button)
                                   (widget-checkbox-action button)))))
                   " None ")
    ;; A menu to define the set operation for the following buttons.
    (insert ?\s ?\s)
    (let* ((checked (widget-get widget :hare-checked))
	   (operation (apply #'widget-create 'menu-choice
			     :value (if checked 'and 'or)
			     :format "%[ %v %]"
			     :menu-tag "Set Operation"
			     '((const
				:value or
				:format "%t"
				:tag "OR"
				:menu-tag "Union/OR")
			       (const
				:value and
				:format "%t"
				:tag "AND"
				:menu-tag "Intersection/AND")
			       (const
				:value and-not
				:format "%t"
				:tag "AND NOT"
				:menu-tag "Difference/AND NOT")
			       (const
				:value xor
				:format "%t"
				:tag "XOR"
				:menu-tag "Symmetric Difference/XOR")))))
      (widget-put widget :hare-set-operation operation))
    (insert ?\s ?\s)
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
			     (let ((operation (widget-get hare--paths-widget :hare-set-operation))
				   (checklist (widget-get hare--paths-widget :hare-checklist)))
			       (let ((op (widget-get operation :value)))
				 (dolist (child (widget-get checklist :children))
				   (hare--paths-widget-apply-set-operation op
				     (not (hare--path-directory-p
					   (widget-get child :value)))
				     (widget-get child :button))))))
                   " Files ")
    (insert ?\s)
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
			     (let ((operation (widget-get hare--paths-widget :hare-set-operation))
				   (checklist (widget-get hare--paths-widget :hare-checklist)))
			       (let ((op (widget-get operation :value)))
				 (dolist (child (widget-get checklist :children))
				   (hare--paths-widget-apply-set-operation op
				     (hare--path-directory-p
				      (widget-get child :value))
				     (widget-get child :button))))))
                   " Folders ")
    (when (hare--paths-vc-state paths)
      (insert ?\s)
      (apply #'widget-create 'menu-choice
	     :format "%[ %t %]"
	     :tag "VC State"
	     :notify (lambda (widget &rest _ignore)
		       (let ((operation (widget-get hare--paths-widget :hare-set-operation))
			     (checklist (widget-get hare--paths-widget :hare-checklist)))
			 (let ((state (widget-get widget :value))
			       (op (widget-get operation :value)))
			   (dolist (child (widget-get checklist :children))
			     (hare--paths-widget-apply-set-operation op
			       (eq state (hare--path-vc-state
					  (widget-get child :value)))
			       (widget-get child :button))))))
	     (mapcar (lambda (state)
		       `(const
			 :value ,state
			 :format "%t"
			 :tag ,(plist-get (cdr (assq state hare--vc-state-alist)) :help)))
		     hare--vc-states)))
    (insert ?\n ?\n)
    ;; The file name listing.
    (insert (directory-file-name (abbreviate-file-name (hare--paths-parent paths))) ?: ?\n)
    (let ((checklist (apply #'widget-create 'checklist
			    :entry-format " %b %v\n"
			    (mapcar (lambda (path)
				      `(hare--path-widget :value ,path))
				    (hare--paths-children paths)))))
      (let ((flag (not (null (widget-get widget :hare-checked)))))
	(dolist (button (widget-get checklist :buttons))
          (unless (eq (widget-value button) flag)
            (widget-checkbox-action button))))
      (widget-put widget :hare-checklist checklist))))

(defun hare--paths-widget-apply-set-operation (operation condition button)
  "Apply a set operation.

The order of the operands is reversed.  The button value is the
left hand side operand and the condition value is the right hand
side operand.  However, this is only relevant for the AND NOT
operation."
  (declare (indent 1))
  (cl-ecase operation
    (or
     ;;    Button: T T F F
     ;;        OR: T T T F
     ;; Condition: T F T F
     (when (and condition (not (widget-value button)))
       (widget-checkbox-action button)))
    (and
     ;;    Button: T T F F
     ;;       AND: T F F F
     ;; Condition: T F T F
     (when (and (not condition) (widget-value button))
       (widget-checkbox-action button)))
    (and-not
     ;;    Button: T T F F
     ;;   AND NOT: F T F F
     ;; Condition: T F T F
     (when (and condition (widget-value button))
       (widget-checkbox-action button)))
    (xor
     ;;    Button: T T F F
     ;;       XOR: F T T F
     ;; Condition: T F T F
     (when condition
       (widget-checkbox-action button))))
  ())

(defun hare--paths-widget-value-get (widget)
  "Return a HareSVN paths structure with all selected children."
  (let ((paths (hare--copy-paths (widget-get widget :value))))
    (setf (hare--paths-children paths)
	  (let (list)
	    (dolist (child (widget-get (widget-get widget :hare-checklist) :children))
	      (when (widget-value (widget-get child :button))
		(push (widget-get child :value) list)))
	    (nreverse list)))
    paths))

(defun hare--paths-widget-insert-subdir ()
  "Insert the files and directories of the subdirectory at point.
See ‘hare--path-widget-directory-keymap’."
  (interactive)
  (hare--paths-widget-apply-subdir-function #'hare--paths-insert-subdir))

(defun hare--paths-widget-insert-subdir-event (event)
  "Insert the files and directories of the indicated subdirectory.
See ‘hare--path-widget-directory-keymap’"
  (interactive "e")
  (save-excursion
    (goto-char (posn-point (event-start event)))
    (hare--paths-widget-insert-subdir)))

(defun hare--paths-widget-remove-subdir ()
  "Remove the files and directories of the subdirectory at point.
See ‘hare--path-widget-directory-keymap’"
  (interactive)
  (hare--paths-widget-apply-subdir-function #'hare--paths-remove-subdir))

(defun hare--paths-widget-remove-subdir-event (event)
  "Remove the files and directories of the indicated subdirectory.
See ‘hare--path-widget-directory-keymap’"
  (interactive "e")
  (save-excursion
    (goto-char (posn-point (event-start event)))
    (hare--paths-widget-remove-subdir)))

(defun hare--paths-widget-apply-subdir-function (fun)
  "Insert or remove the files and directories of the subdirectory at point."
  (when-let* ((path-widget (hare--path-widget-at-point))
	      (path (widget-get path-widget :value))
	      (paths-widget hare--paths-widget)
	      (paths (widget-get paths-widget :value))
	      (checklist (widget-get paths-widget :hare-checklist)))
    (when (funcall fun paths path)
      (let (;; Backup button values.
	    (checked (mapcar (lambda (child)
			       (let ((path (widget-get child :value)))
				 (cons (hare--path-absolute path)
				       (widget-value (widget-get child :button)))))
			     (widget-get checklist :children))))
	;; Replace the ‘checklist’ widget.
	(widget-apply checklist :delete)
	;; TODO: Is it possible to modify the ‘checklist’ widget
	;; in place instead of creating a new one?  Otherwise,
	;; sync the code with ‘hare--paths-widget-value-create’.
	(setq checklist (apply #'widget-create 'checklist
			       :entry-format " %b %v\n"
			       (mapcar (lambda (path)
					 `(hare--path-widget :value ,path))
				       (hare--paths-children paths))))
	;; Restore button values.
	(dolist (child (widget-get checklist :children))
	  (let* ((button (widget-get child :button))
		 (path (widget-get child :value))
		 (cell (cl-assoc (hare--path-absolute path) checked
				 :test #'hare--file-name-equal)))
	    (when (and cell (not (eq (widget-value button) (cdr cell))))
	      (widget-checkbox-action button))))
	;; Update the paths widget.
	(widget-put paths-widget :hare-checklist checklist)))))

(defun hare--fileset-from-paths (object &rest options)
  "Create a VC fileset from a paths object.

If keyword argument NOT-EMPTY is non-nil,
 return nil if the fileset is empty.
If keyword argument NO-MESSAGE is non-nil,
 don't print a status message.
If keyword argument RELATIVE is non-nil,
 gather relative file names.

Return nil if no fileset can be determined."
  (if-let* ((type (cond ((widgetp object)
			 (widget-type object))
			((hare--paths-p object)
			 'hare--paths)))
	    (paths (cl-case type
		     (hare--paths-widget
		      (widget-get object :value))
		     (hare--paths
		      object)))
	    (backend (or (hare--paths-vc-backend paths)
			 (vc-responsible-backend
			  (hare--paths-vc-root paths) t)
			 (vc-responsible-backend
			  (hare--paths-parent paths) t)))
	    (file-name (if (plist-get options :relative)
			   #'hare--path-relative
			 #'hare--path-absolute)))
      (let ((files (cl-case type
		     (hare--paths-widget
		      (let (list)
			(dolist (child (widget-get (widget-get object :hare-checklist) :children))
			  (when (widget-value (widget-get child :button))
			    (push (funcall file-name (widget-value child)) list)))
			(nreverse list)))
		     (hare--paths
		      (mapcar file-name (hare--paths-children object))))))
	(if (and (null files) (plist-get options :not-empty))
	    (unless (plist-get options :no-message)
	      (ignore (message "No files")))
	  (list backend files)))
    (unless (plist-get options :no-message)
      (ignore (message "Can not determine any file")))))

(defvar-local hare--log-edit-widget nil
  "The widget for editing the log message.")

(defun hare--log-edit-widget-previous-comment (arg)
  "Like ‘log-edit-previous-comment’."
  (interactive "*p")
  (when-let ((log-edit-widget hare--log-edit-widget))
    (with-temp-buffer
      (log-edit-previous-comment arg)
      (when (buffer-modified-p)
	(hare--trim-log-message)
	(widget-value-set log-edit-widget (buffer-substring-no-properties
					   (point-min) (point-max)))))))

(defun hare--log-edit-widget-next-comment (arg)
  "Like ‘log-edit-next-comment’."
  (interactive "*p")
  (when-let ((log-edit-widget hare--log-edit-widget))
    (with-temp-buffer
      (log-edit-next-comment arg)
      (when (buffer-modified-p)
	(hare--trim-log-message)
	(widget-value-set log-edit-widget (buffer-substring-no-properties
					   (point-min) (point-max)))))))

(defun hare--log-edit-widget-show-diff ()
  "Like ‘log-edit-show-diff’."
  (interactive)
  (when-let* ((paths-widget hare--paths-widget)
	      (fileset (hare--fileset-from-paths paths-widget :not-empty t)))
    (vc-diff-internal nil fileset nil nil)))

(defun hare--log-edit-widget-show-files ()
  "Like ‘log-edit-show-files’."
  (interactive)
  (when-let* ((paths-widget hare--paths-widget)
	      (fileset (hare--fileset-from-paths paths-widget :not-empty t)))
    (let ((log-edit-listfun (lambda () (cl-second fileset))))
      (log-edit-show-files))))

(defun hare--log-edit-widget-insert-changelog (&optional use-first)
  "Like ‘log-edit-insert-changelog’."
  (interactive "P")
  (when-let* ((log-edit-widget hare--log-edit-widget)
	      (paths-widget hare--paths-widget)
	      (fileset (hare--fileset-from-paths paths-widget :not-empty t :relative t)))
    (with-temp-buffer
      (let ((log-edit-listfun (lambda () (cl-second fileset))))
	(log-edit-insert-changelog use-first))
      (hare--trim-log-message)
      (widget-value-set log-edit-widget (buffer-substring-no-properties
					 (point-min) (point-max))))))

(defun hare--log-edit-widget-generate-changelog-from-diff ()
  "Like ‘log-edit-generate-changelog-from-diff’."
  (interactive)
  (when-let* ((log-edit-widget hare--log-edit-widget)
	      (paths-widget hare--paths-widget)
	      (fileset (hare--fileset-from-paths paths-widget :not-empty t)))
    (with-temp-buffer
      (change-log-insert-entries
       (with-temp-buffer
	 (vc-diff-internal nil fileset nil nil nil (current-buffer))
	 (diff-add-log-current-defuns)))
      (hare--trim-log-message)
      (widget-value-set log-edit-widget (buffer-substring-no-properties
					 (point-min) (point-max))))))

(defvar hare--log-edit-widget-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-text-keymap)
    (define-key map (kbd "M-p") 'hare--log-edit-widget-previous-comment)
    (define-key map (kbd "M-n") 'hare--log-edit-widget-next-comment)
    (define-key map (kbd "C-c C-d") 'hare--log-edit-widget-show-diff)
    (define-key map (kbd "C-c C-f") 'hare--log-edit-widget-show-files)
    (define-key map (kbd "C-c C-a") 'hare--log-edit-widget-insert-changelog)
    (define-key map (kbd "C-c C-w") 'hare--log-edit-widget-generate-changelog-from-diff)
    (define-key map (kbd "C-c C-k") 'hare-cancel-form)
    (define-key map (kbd "C-c C-c") 'hare-submit-form)
    map)
  "Keymap for editing log messages in a form.")

(defun hare--log-edit-widget-apply-command (command)
  "Apply a log message command."
  (when-let ((log-edit-widget hare--log-edit-widget))
    (cl-ecase command
      (edit-clear
       ;; Clear log message.
       (widget-value-set log-edit-widget ""))
      (edit-trim
       ;; Delete superfluous whitespace.
       (let* ((old (widget-value log-edit-widget))
	      (new (with-temp-buffer
		     (insert old)
		     (hare--trim-log-message)
		     (buffer-substring (point-min) (point-max)))))
	 (unless (string-equal old new)
	   (widget-value-set log-edit-widget new))))
      (tools-diff
       ;; Show file differences.
       (hare--log-edit-widget-show-diff))
      (tools-files
       ;; Show file names.
       (hare--log-edit-widget-show-files))
      ())))

(define-widget 'hare--log-edit-widget 'default
  "A widget for editing a log message."
  :format "%v"
  :value-create 'hare--log-edit-widget-value-create
  :value-set 'hare--log-edit-widget-value-set
  :value-get 'hare--log-edit-widget-value-get
  ;; Embedded widgets.
  :hare-text nil)

(defun hare--log-edit-widget-value-create (widget)
  "Insert the printed representation of the value."
  (let ((message (or (widget-get widget :value) "")))
    (unless (stringp message)
      (error "Invalid value"))
    (setq hare--log-edit-widget widget)
    (insert "Log Message: ")
    (apply #'widget-create 'menu-choice
	   :format "%[ %t %]"
	   :tag "Edit"
	   :notify (lambda (widget &rest _ignore)
		     (hare--log-edit-widget-apply-command
		      (widget-get widget :value)))
	   '((const
	      :value edit-clear
	      :format "%t"
	      :tag "Clear Log Message")
	     (const
	      :value edit-trim
	      :format "%t"
	      :tag "Delete Superfluous Whitespace")))
    (insert ?\s)
    (apply #'widget-create 'menu-choice
	   :format "%[ %t %]"
	   :tag "Tools"
	   :notify (lambda (widget &rest _ignore)
		     (hare--log-edit-widget-apply-command
		      (widget-get widget :value)))
	   '((const
	      :value tools-diff
	      :format "%t"
	      :tag "Show File Differences")
	     (const
	      :value tools-files
	      :format "%t"
	      :tag "Show File Names")))
    (insert ?\n ?\n)
    (let ((text (widget-create 'text
			       :value message
			       :format "%v"
			       :keymap hare--log-edit-widget-keymap)))
      (widget-put widget :hare-text text))))

(defun hare--log-edit-widget-value-set (widget value)
  "Set the log message of WIDGET to VALUE."
  (widget-value-set (widget-get widget :hare-text) value))

(defun hare--log-edit-widget-value-get (widget)
  "Return the log message of WIDGET."
  (widget-value (widget-get widget :hare-text)))

(defconst hare--date-regexp
  (rx-let ((year (or (and ?0 (= 3 (char "0-9"))) (and (char "1-9") (>= 3 (char "0-9")))))
           (month (or (and ?0 (char "0-9")) (and ?1 (char "0-2"))))
           (day (or (and (char "0-2") (char "0-9")) (and ?3 (char "0-1"))))
           (hour (or (and (char "0-1") (char "0-9")) (and ?2 (char "0-3"))))
           (minute (and (char "0-5") (char "0-9")))
           (second (and (char "0-5") (char "0-9"))))
    (rx (or
         ;; Basic format.
         (and year month day
              (? (and ?T
                      hour (? (and minute (? second)))
                      (? (or ?Z (and (char ?+ ?-) hour (? minute)))))))
         ;; Extended format.
         (and year ?- month ?- day
              (? (and (or (+ ?\s) ?T)
                      hour (? (and ?: minute (? (and ?: second))))
                      (? (or ?Z (and (char ?+ ?-) hour (? (and ":" minute)))))))))))
  "Regexp matching an ISO 8601 date in basic or extended format.")

(defun hare--date (time &optional days)
  "Return the ISO 8601 date for TIME.

Optional argument DAYS is the number of days before or after TIME.
A positive value counts forward."
  (when (null time)
    (setq time (current-time)))
  (format-time-string "%+4Y-%m-%d"
		      (if (null days)
			  time
			(time-add time (* days 86400)))))

(defun hare--date-and-time (time &optional seconds)
  "Return the ISO 8601 date for TIME.
The date includes the current time.

If optional argument SECONDS is non-nil, the current time includes the
second of the minute.  Otherwise, the current time is truncated to the
current minute of the hour."
  (when (null time)
    (setq time (current-time)))
  (format-time-string (if seconds
			  "%+4Y-%m-%d %H:%M:%S"
			"%+4Y-%m-%d %H:%M")
		      time))

(defun hare--insert-revision-date (arg)
  "Insert the revision date for today at point.

A numeric prefix argument specifies the number of days before or after
today.  A positive value counts forward.  A non-numeric prefix argument
means to include the current time.

The revision date is rounded up to the next time unit."
  (interactive "P")
  (let* ((time (current-time))
	 (date (if (consp arg)
		   (let ((seconds (> (car arg) 4)))
		     (hare--date-and-time
		      (time-add time (if seconds 1 60)) seconds))
		 (hare--date
		  time (1+ (if (null arg) 0 (prefix-numeric-value arg)))))))
    (if-let ((widget (widget-field-at (point))))
	(widget-value-set widget date)
      (barf-if-buffer-read-only)
      (insert date))))

(defvar hare--revision-date-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-text-keymap)
    (define-key map (kbd "C-c .") 'hare--insert-revision-date)
    map)
  "Keymap for a revision date widget.")

(define-widget 'hare--svn-revision-date 'string
  "A Subversion revision date widget."
  :tag "Revision Date"
  :size 32 ;{YYYY-MM-DD hh:mm:ss.sss±hh:mm}
  :valid-regexp (concat "\\`{?" hare--date-regexp "}?\\'")
  :keymap hare--revision-date-keymap
  :value-get 'hare--svn-revision-date-value-get)

(defun hare--svn-revision-date-value-get (widget)
  "Return the value of WIDGET."
  (let ((date (string-trim (widget-get widget :value))))
    (unless (string-prefix-p "{" date)
      (setq date (concat "{" date)))
    (unless (string-suffix-p "}" date)
      (setq date (concat date "}")))
    date))

;; See https://svnbook.red-bean.com/en/1.7/svn.tour.revs.specifiers.html.
(define-widget 'hare--svn-revision 'menu-choice
  "A widget for selecting a specific Subversion revision."
  :value nil
  :format "%t %[ Value Menu %]: %v"
  :tag "Revision"
  :help-echo "Specify a revision"
  :args `((const
	   :value nil
	   :format "%t\n%h"
	   :menu-tag "None"
	   :doc "The revision is not specified.")
	  (natnum
	   :value 1
	   :format "%{%t%}: %v\n%h"
	   :tag "Number"
	   :size 10
	   :doc "The revision number.")
	  (hare--svn-revision-date
	   :format "%{%t%}: %v\n%h"
	   :tag "Date"
	   :doc "The most recent revision as of that date.")
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
	   :doc "The revision before ‘COMMITTED’.")))

(define-widget 'hare--svn-revision-choice 'radio-button-choice
  "A widget for selecting a Subversion revision range."
  :value nil
  :entry-format " %b %v"
  :args `((const
	   :value nil
	   :format "%t\n\n"
	   :tag "Local modifications.")
	  (group
	   :value (change nil)
	   :format "%v\n"
	   (const
	    :value change
	    :format "%t\n"
	    :tag "Changes made by a revision.")
	   (menu-choice
	    :format "%t %[ Value Menu %]: %v"
	    :tag "Revision"
	    :help-echo "Specify a revision"
	    (const
	     :value nil
	     :format "%t\n%h"
	     :menu-tag "None"
	     :doc "The revision is not specified.")
	    (natnum
	     :value 1
	     :format "%{%t%}: %v\n%h"
	     :tag "Number"
	     :size 10
	     :doc "The revision number.")))
	  (group
	   :value (revision BASE nil)
	   :format "%v"
	   (const
	    :value revision
	    :format "%t\n"
	    :tag "Differences between two revisions.")
	   (hare--svn-revision
	    :tag "From Revision")
	   (hare--svn-revision
	    :tag "To Revision"))))

(define-widget 'hare--svn-accept 'menu-choice
  "A widget for selecting the conflict resolution action."
  :value 'postpone
  :format "%t %[ Value Menu %]: %v"
  :tag "Conflict Resolution"
  :help-echo "Define the action for automatic conflict resolution"
  :args `((const
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
content.")))

(define-widget 'hare--svn-auto-props 'menu-choice
  "A widget for selecting the automatic property assignment."
  :value 'undefined
  :format "%t %[ Value Menu %]: %v"
  :tag "Automatic Property Assignment"
  :help-echo "Override the runtime configuration directive for automatic property assignment"
  :args `((const
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
Override the ‘enable-auto-props’ runtime configuration directive.")))

(define-widget 'hare--svn-depth 'menu-choice
  "A widget for selecting the operational depth."
  :value nil
  :format "%t %[ Value Menu %]: %v"
  :tag "Operational Depth"
  :help-echo "Limit the scope of the operation"
  :args `((const
	   :value nil
	   :format "%t\n%d"
	   :menu-tag "None"
	   :doc "\
Apply the default behavior of the operation.")
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
children, its children's children, and so on to full recursion.")))

(define-widget 'hare--svn-set-depth 'menu-choice
  "A widget for setting the operational depth."
  :value nil
  :format "%t %[ Value Menu %]: %v"
  :tag "Set Sticky Depth"
  :help-echo "Change the scope (sticky depth) of a directory in the working copy"
  :args `((const
	   :value nil
	   :format "%t\n%d"
	   :menu-tag "None"
	   :doc "\
Don't change the scope (sticky depth) of a directory in the working copy.")
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
	  (const
	   :value exclude
	   :format "%t\n%d"
	   :menu-tag "Exclude"
	   :doc "\
Exclude the immediate target of the operation from the working copy.")))

(define-widget 'hare--svn-limit 'menu-choice
  "A widget for selecting the number of log messages."
  :value nil
  :format "%t %[ Value Menu %]: %v"
  :tag "Limit"
  :help-echo "Specify the number of log messages"
  :args `((const
	   :value nil
	   :format "%t\n%h"
	   :menu-tag "None"
	   :doc "The number of log messages is not specified.")
	  (natnum
	   :value 1
	   :format "%{%t%}: %v\n%h"
	   :tag "Number"
	   :size 10
	   :doc "The maximum number of log messages to process.")))

(defun hare--create-svn-widget (type &rest options)
  "Insert a Subversion widget into the form.
Return value is the widget handle."
  (declare (indent 1))
  ;; See https://svnbook.red-bean.com/en/1.7/svn.ref.svn.html.
  (cl-case type
    (revision
     (apply #'widget-create 'hare--svn-revision options))
    (revision-choice
     (apply #'widget-create 'hare--svn-revision-choice options))
    (accept
     (apply #'widget-create 'hare--svn-accept options))
    (auto-props
     (apply #'widget-create 'hare--svn-auto-props options))
    (depth
     ;; See https://svnbook.red-bean.com/en/1.7/svn.advanced.sparsedirs.html.
     (apply #'widget-create 'hare--svn-depth options))
    (set-depth
     (apply #'widget-create 'hare--svn-set-depth options))
    (limit
     (apply #'widget-create 'hare--svn-limit options))
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

;;;###autoload
(define-derived-mode hare-process-mode special-mode "HareSVN-Process"
  "Major mode for HareSVN process buffers."
  :group 'hare
  :syntax-table nil
  :abbrev-table nil
  :interactive nil)

(defcustom hare-delete-process-window '(not svn-log svn-status)
  "Whether or not to delete the process window after an operation.
This option only has an effect if the process succeeds.

If the value is a non-empty list of symbols, it lists the operations
after which the process window shall be deleted.  If the list starts
with the symbol ‘not’, it lists the operations after which the process
window shall not be deleted."
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (set :tag "Operations"
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
Second argument CONDITIONS is a list of symbols.  It encodes the
 processed operation.  See the variable ‘hare-delete-process-window’
 for more details.

This function acts according to the current values of the options
‘hare-delete-process-window’ and ‘hare-delete-process-window-delay’."
  (when (cond ((consp hare-delete-process-window)
	       (let* ((notp (eq (car hare-delete-process-window) 'not))
		      (haystack (if notp
				    (cdr hare-delete-process-window)
				  hare-delete-process-window)))
		 (funcall (if notp #'cl-notany #'cl-some)
			  (lambda (needle)
			    (memq needle haystack))
			  conditions)))
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
	    (,status (progn
		       (with-current-buffer ,buffer
			 (hare-process-mode))
		       ,@body)))
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

(defun hare--string (object &optional numeric)
  "Return a string described by OBJECT.

Emacs has no distinct type for characters.  If optional second argument
NUMERIC is non-nil, treat a character as a numeric type."
  (cl-typecase object
    (string
     object)
    (character
     (if numeric
	 (with-output-to-string
	   (princ object))
       (string object)))
    (symbol
     (symbol-name object))
    (t
     (with-output-to-string
       (princ object)))))

;;;; Subversion

(defun hare--svn-collect-paths (&rest options)
  "Collect paths, i.e. files and directories, for a Subversion command.
Signal an error if not within a working copy.

See ‘hare--collect-paths’ for the meaning of OPTIONS."
  (apply #'hare--collect-paths :vc-backend 'SVN :vc-root t
	 ;; Keyword argument VC-STATE defaults to t.
	 (if (memq :vc-state options)
	     options
	   (cl-list* :vc-state t options))))

(defun hare--svn (buffer success targets command &rest options)
  "Execute a ‘svn’ command.

First argument BUFFER is the process buffer.  Value is either a
 buffer or a cons cell of the form ‘(BUFFER . BUFFER-OPTIONS)’
 where BUFFER-OPTIONS is a property list.
Second argument SUCCESS is the process status indicating a successful
 command execution.
Third argument TARGETS are the targets for the Subversion command.
 Value is either a list of absolute file names or a HareSVN paths
 structure.
Fourth argument COMMAND is the Subversion command to be executed.
Remaining arguments OPTIONS are any additional command line options.

Return non-nil if the command succeeds."
  (unless (listp vc-svn-global-switches)
    (error "User option ‘vc-svn-global-switches’ is not a list, please fix it"))
  (let ((came-from (current-buffer))
	(buffer-options ())
	(status nil))
    (when (consp buffer)
      (setq buffer-options (cdr buffer)
	    buffer (car buffer)))
    (save-selected-window
      (with-current-buffer buffer
	(let ((inhibit-read-only t)
	      (comint-file-name-quote-list shell-file-name-quote-list))
	  (let* ((mode (or (plist-get buffer-options :major-mode) 'hare-process-mode))
		 (mode-name (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
	    (unless (eq major-mode mode)
	      (funcall mode))
	    (goto-char (point-max))
	    (when (bobp)
	      ;; Initial setup.
	      (let ((dir (plist-get buffer-options :default-directory)))
		(cond ((stringp dir)
		       (setq default-directory (file-name-as-directory dir)))
		      ((hare--paths-p targets)
		       (when-let ((root (hare--paths-vc-root targets)))
			 (setq default-directory (hare--paths-vc-root targets)))))))
	    (when (and (bobp) (not (plist-get buffer-options :omit-first-line)))
	      (insert "-*- mode: " mode-name "; ")
	      (when (local-variable-p 'default-directory)
		(insert "default-directory: "
			(with-output-to-string (prin1 default-directory))
			"; "))
	      (insert "-*-" ?\n ?\n)))
	  (unless (bolp)
	    (insert ?\n))
	  (when (hare--paths-p targets)
	    (let ((parent (hare--paths-parent targets)))
	      (insert "cd" ?\s (comint-quote-filename parent) ?\n)
	      (cd parent)))
	  (let ((arguments (append vc-svn-global-switches
				   (when command (list command))
				   options
				   (cond ((hare--paths-p targets)
					  (mapcar #'hare--path-relative
						  (hare--paths-children targets)))
					 ((listp targets)
					  (mapcar #'expand-file-name targets))
					 ((stringp targets)
					  (list (expand-file-name targets)))))))
	    ;; Show the command line.
	    (insert (comint-quote-filename vc-svn-program))
	    (dolist (argument arguments)
	      (insert ?\s (comint-quote-filename argument)))
	    (insert ?\n ?\n)
	    (let ((mark (point)))
	      (setq status (let ((output (plist-get buffer-options :output-buffer)))
			     (if (not (buffer-live-p output))
				 ;; Insert output into the process buffer.
				 (setq output buffer)
			       (insert "[Output diverted to buffer ‘")
			       (insert-text-button
				(buffer-name output)
				'action (lambda (&rest _ignore)
					  (switch-to-buffer output))
				'follow-link t)
			       (insert "’]")
			       (insert ?\n))
			     (ignore-errors
			       (apply #'call-process vc-svn-program nil (list output t) t arguments))))
	      (unless (bolp)
		(insert ?\n))
	      (when (< mark (point))
		(insert ?\n))))
	  (cond ((null status)
		 (insert (propertize "Failure:"
				     'face 'error
				     'font-lock-face 'error)
			 " internal error"))
		((> status success)
		 (insert (propertize "Failure:"
				     'face 'error
				     'font-lock-face 'error)
			 (format " exit status %s" status))
		 ;; Clear return value.
		 (setq status nil))
		(t
		 (insert (propertize "Success:"
				     'face 'success
				     'font-lock-face 'success)
			 (format " exit status %s" status))))
	  (insert ?\n))))
    ;; Update VC states.
    (let ((backend 'SVN))
      (cond ((hare--paths-p targets)
	     (dolist (child (hare--paths-children targets))
	       (vc-state-refresh (hare--path-absolute child) backend)))
	    ((listp targets)
	     (dolist (target targets)
	       (vc-state-refresh target backend)))
	    ((stringp targets)
	     (vc-state-refresh targets backend))))
    (when (buffer-live-p came-from)
      (with-current-buffer came-from
	(cond ((derived-mode-p 'dired-mode)
	       (revert-buffer)))))
    ;; Return value.
    status))

(defun hare--svn-commit (targets message &rest options)
  "Run the ‘svn commit’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Second argument MESSAGE is the mandatory log message.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-log-message-file (file message)
    (hare--with-process-window (buffer '(svn-commit))
      (apply #'hare--svn buffer 0 targets "commit"
	     (nconc (list "--file" file "--force-log")
		    (when (plist-get options :quiet)
		      (list "--quiet"))
		    (when (plist-get options :no-unlock)
		      (list "--no-unlock"))
		    (when (plist-get options :include-externals)
		      (list "--include-externals"))
		    (when-let ((depth (plist-get options :depth)))
		      (list "--depth" (hare--string depth)))
		    ())))))

(defun hare-svn-commit ()
  "Send changes from your working copy to the repository."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--form (message targets depth no-unlock externals)
	"Send changes from your working copy to the repository."
	(hare--svn-commit targets message
			  :depth depth
			  :no-unlock no-unlock
			  :include-externals externals)
      (setq no-unlock (hare--create-svn-widget 'checkbox
			:doc "Don't unlock targets.
The default is to unlock any locked target after a successful commit."))
      (insert ?\n)
      (setq externals (hare--create-svn-widget 'checkbox
			:doc "Include external definitions.
Also operate on externals defined by ‘svn:externals’ properties.
Don't commit externals with a fixed revision."))
      (insert ?\n)
      (setq depth (hare--create-svn-widget 'depth :value 'empty))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      (hare--form-horizontal-line)
      (setq message (widget-create 'hare--log-edit-widget))
      ())))

(defun hare--svn-update (targets &rest options)
  "Run the ‘svn update’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-process-window (buffer '(svn-update))
    (apply #'hare--svn buffer 0 targets "update"
	   (nconc (when (plist-get options :quiet)
		    (list "--quiet"))
		  (when-let ((revision (plist-get options :revision)))
		    (list "--revision" (hare--string revision t)))
		  (when-let ((accept (plist-get options :accept)))
		    (list "--accept" (hare--string accept)))
		  (when (plist-get options :force)
		    (list "--force"))
		  (when (plist-get options :parents)
		    (list "--parents"))
		  (when (plist-get options :ignore-externals)
		    (list "--ignore-externals"))
		  (when-let ((set-depth (plist-get options :set-depth)))
		    (list "--set-depth" (hare--string set-depth)))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  ()))))

(defun hare-svn-update ()
  "Update your working copy."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--form (targets depth set-depth revision accept force parents externals)
	"Update your working copy.

Synchronize the working copy to the given revision.  If no revision is
specified, synchronize the working copy to the latest revision in the
repository."
	(hare--svn-update targets
			  :depth depth
			  :set-depth set-depth
			  :revision revision
			  :accept accept
			  :force force
			  :parents parents
			  :ignore-externals externals)
      (setq revision (widget-create 'hare--svn-revision))
      (insert ?\n)
      (setq accept (hare--create-svn-widget 'accept))
      (insert ?\n)
      (setq force (hare--create-svn-widget 'checkbox
		    :doc "Handle unversioned obstructions as changes.
If enabled, unversioned paths in the working copy do not automatically
cause a failure if the update attempts to add the same path.  If the
unversioned path is the same type (file or directory) as the path in
the repository, it becomes versioned but its content is left as-is in
the working copy.  This means that an obstructing directory's unversioned
children may also obstruct and become versioned.  For files, any content
differences between the obstruction and the repository are treated like
a local modification to the working copy.  All properties from the
repository are applied to the obstructing path."
		    :value t))
      (insert ?\n)
      (setq parents (hare--create-svn-widget 'checkbox
		      :doc "Create intermediate directories.
If a target is missing in the working copy but its immediate parent
directory is present, checkout the target into its parent directory
at the specified depth.  If this option is enabled, create any missing
parent directories of the target by checking them out at depth ‘empty’,
too."
		      :value t))
      (insert ?\n)
      (setq externals (hare--create-svn-widget 'checkbox
			:doc "Ignore external definitions.
Don't operate on externals defined by ‘svn:externals’ properties."))
      (insert ?\n)
      (setq set-depth (hare--create-svn-widget 'set-depth))
      (insert ?\n)
      (setq depth (hare--create-svn-widget 'depth))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-resolve (targets &rest options)
  "Run the ‘svn resolve’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-process-window (buffer '(svn-resolve))
    (apply #'hare--svn buffer 0 targets "resolve"
	   (nconc (when (plist-get options :quiet)
		    (list "--quiet"))
		  (when-let ((accept (plist-get options :accept)))
		    (list "--accept" (hare--string accept)))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  ()))))

(defun hare-svn-resolve ()
  "Resolve conflicts on working copy files or directories."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--form (targets depth accept)
	"Resolve conflicts on working copy files or directories."
	(hare--svn-resolve targets
			   :depth depth
			   :accept accept)
      (setq accept (hare--create-svn-widget 'accept
		     :value 'working))
      (insert ?\n)
      (setq depth (hare--create-svn-widget 'depth :value 'empty))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-add (targets &rest options)
  "Run the ‘svn add’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-process-window (buffer '(svn-add))
    (apply #'hare--svn buffer 0 targets "add"
	   (nconc (when (plist-get options :quiet)
		    (list "--quiet"))
		  (when (plist-get options :no-ignore)
		    (list "--no-ignore"))
		  (when (plist-get options :auto-props)
		    (list "--auto-props"))
		  (when (plist-get options :no-auto-props)
		    (list "--no-auto-props"))
		  (when (plist-get options :force)
		    (list "--force"))
		  (when (plist-get options :parents)
		    (list "--parents"))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  ()))))

(defun hare-svn-add ()
  "Put files and directories under version control."
  (interactive)
  (let ((paths (hare--svn-collect-paths
		:vc-state '(unregistered nil)
		:parent-items t)))
    (hare--form (targets depth no-ignore auto-props force parents)
	"Put files and directories under version control."
	(hare--svn-add targets
		       :depth depth
		       :no-ignore no-ignore
		       :auto-props (eq auto-props t)
		       :no-auto-props (eq auto-props nil)
		       :force force
		       :parents parents)
      (setq no-ignore (hare--create-svn-widget 'checkbox
			:doc "Don't apply ignore rules to implicitly added items.
Subversion uses ignore patterns to determine which items should be
skipped as part of a larger recursive operation.  If this option is
enabled, operate on all the files and directories present."))
      (insert ?\n)
      (setq auto-props (hare--create-svn-widget 'auto-props))
      (insert ?\n)
      (setq force (hare--create-svn-widget 'checkbox
		    :doc "Ignore already versioned paths.
If this option is enabled, add all the unversioned paths and ignore
the rest.  Otherwise, error out if a path is already versioned."
		    :value t))
      (insert ?\n)
      (setq parents (hare--create-svn-widget 'checkbox
		      :doc "Add intermediate directories.
If this option is enabled, add any missing parent directories of the
target at depth ‘empty’, too."
		      :value t))
      (insert ?\n)
      (setq depth (hare--create-svn-widget 'depth :value 'empty))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-delete (targets &rest options)
  "Run the ‘svn delete’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (let ((message (plist-get options :message)))
    (hare--with-log-message-file (file message)
      (hare--with-process-window (buffer '(svn-delete))
	(apply #'hare--svn buffer 0 targets "delete"
	       (nconc (when (not (null message))
			(list "--file" file "--force-log"))
		      (when (plist-get options :quiet)
			(list "--quiet"))
		      (when (plist-get options :force)
			(list "--force"))
		      (when (plist-get options :keep-local)
			(list "--keep-local"))
		      ()))))))

(defun hare-svn-delete ()
  "Remove files and directories from version control."
  (interactive)
  (let ((paths (hare--svn-collect-paths
		:vc-state '(not removed unregistered nil)
		:parent-items t)))
    (hare--form (targets force)
	"Remove files and directories from version control.

Files and directories are only scheduled for removal.
The actual removal occurs upon the next commit."
	(hare--svn-delete targets
			  :force force
			  :keep-local t) ;schedule for removal
      (setq force (hare--create-svn-widget 'checkbox
		    :doc "Remove modified files and directories.
If enabled, files and directories are removed regardless of their
version control state.  Otherwise, modified items are not removed."))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-revert (targets &rest options)
  "Run the ‘svn revert’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-process-window (buffer '(svn-revert))
    (apply #'hare--svn buffer 0 targets "revert"
	   (nconc (when (plist-get options :quiet)
		    (list "--quiet"))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  ()))))

(defun hare-svn-revert ()
  "Undo local modifications."
  (interactive)
  (let ((paths (hare--svn-collect-paths
		:vc-state '(not up-to-date unregistered nil)
		:parent-items t)))
    (hare--form (targets depth)
	"Undo local modifications."
	(hare--svn-revert targets
			  :depth depth)
      (setq depth (hare--create-svn-widget 'depth :value 'empty))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defcustom hare-svn-diff-output "*vc-diff*"
  "Destination for the output of the ‘svn diff’ command.
If the value is nil, do not divert the process output, i.e. insert it
into the process buffer.  For this case, you might want to exclude the
‘svn-diff’ operation from the variable ‘hare-delete-process-window’ so
that the window displaying the process buffer persists.
If the value is a string, divert the output to the buffer of this name."
  :type '(choice (const :tag "Embedded" nil)
		 (string :tag "Buffer Name"))
  :group 'hare)

(defun hare--svn-diff (targets &rest options)
  "Run the ‘svn diff’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (if (plist-get options :summarize)
      ;; Output is similar to an ‘svn status’ command.
      ;; Thus, treat it like that.
      (hare--with-process-window (buffer '(svn-status))
	(apply #'hare--svn buffer 0 targets "diff"
	       (nconc (list "--summarize")
		      (when-let ((revision (plist-get options :revision)))
			(list "--revision" (hare--string revision t)))
		      (when-let ((change (plist-get options :change)))
			(list "--change" (hare--string change t)))
		      (when-let ((extensions (plist-get options :extensions)))
			(list "--extensions" (hare--string extensions)))
		      (when (plist-get options :ignore-properties)
			(list "--ignore-properties"))
		      (when (plist-get options :properties-only)
			(list "--properties-only"))
		      (when (plist-get options :notice-ancestry)
			(list "--notice-ancestry"))
		      (when-let ((depth (plist-get options :depth)))
			(list "--depth" (hare--string depth)))
		      ())))
    (let (status process-buffer output-buffer)
      (setq status (hare--with-process-window (buffer '(svn-diff))
		     (setq process-buffer buffer
			   output-buffer buffer)
		     (let ((dest (cl-etypecase hare-svn-diff-output
				   (null
				    ;; Insert output into the process buffer.
				    (nconc (list buffer :omit-first-line t)
					   (when (hare--paths-p targets)
					     (list :default-directory (hare--paths-parent targets)))))
				   (string
				    ;; Send output to a separate output buffer.
				    (let ((tem (get-buffer-create hare-svn-diff-output)))
				      (setq output-buffer tem)
				      (with-current-buffer tem
					(kill-all-local-variables)
					(setq buffer-read-only nil)
					(erase-buffer))
				      (list buffer :output-buffer tem))))))
		       (apply #'hare--svn dest 0 targets "diff"
			      (nconc (when-let ((revision (plist-get options :revision)))
				       (list "--revision" (hare--string revision t)))
				     (when-let ((change (plist-get options :change)))
				       (list "--change" (hare--string change t)))
				     (when-let ((extensions (plist-get options :extensions)))
				       (list "--extensions" (hare--string extensions)))
				     (when (plist-get options :ignore-properties)
				       (list "--ignore-properties"))
				     (when (plist-get options :properties-only)
				       (list "--properties-only"))
				     (when (plist-get options :no-diff-added)
				       (list "--no-diff-added"))
				     (when (plist-get options :no-diff-deleted)
				       (list "--no-diff-deleted"))
				     (when (plist-get options :show-copies-as-adds)
				       (list "--show-copies-as-adds"))
				     (when (plist-get options :notice-ancestry)
				       (list "--notice-ancestry"))
				     (when (plist-get options :force)
				       (list "--force"))
				     (cl-ecase (plist-get options :output-format)
				       ((patch :patch)
					(list "--patch-compatible"))
				       ((git :git)
					(list "--git"))
				       ((nil)
					()))
				     (when-let ((depth (plist-get options :depth)))
				       (list "--depth" (hare--string depth)))
				     ())))))
      (cond ((eq process-buffer output-buffer)
	     (with-current-buffer process-buffer
	       (diff-mode)))
	    (;; Whether or not to display the output buffer.
	     (and status
		  (with-current-buffer output-buffer
		    (goto-char (point-min))
		    (if (save-excursion
			  (skip-chars-forward " \t\n")
			  (eobp))
			(ignore (message "No differences")) ;nil
		      ;; Output buffer is not empty.
		      (when (hare--paths-p targets)
			(setq default-directory (hare--paths-parent targets)))
		      (setq buffer-read-only t)
		      (diff-mode)
		      t)))
	     (let ((display-buffer-alist `((".*" . ,hare--temp-buffer-action))))
	       (display-buffer output-buffer))))
      status)))

(defun hare-svn-diff ()
  "Display local modifications in a working copy."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--svn-diff paths)
    ()))

(defun hare-svn-diff-revisions ()
  "Display differences between two revisions."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--form (targets depth revision properties whitespace newline added deleted copied ancestry output-format)
	"Display differences between two revisions."
	(apply #'hare--svn-diff targets
	       (nconc (list :depth depth)
		      (cl-multiple-value-bind (first second third)
			  (cl-values-list revision)
			(cond ((and (eq first 'change) second)
			       (list :change second))
			      ((and (eq first 'revision) second)
			       (list :revision (if (null third)
						   (hare--string second t)
						 (format "%s:%s" second third))))))
		      (cl-case properties
			(ignore
			 (list :ignore-properties t))
			(only
			 (list :properties-only t)))
		      (when (or whitespace newline)
			(list :extensions (string-trim (concat whitespace " " (when newline "--ignore-eol-style")))))
		      (if (eq output-format 'summary)
			  (list :notice-ancestry ancestry
				:summarize t)
 			(list :no-diff-added added
			      :no-diff-deleted deleted
			      :show-copies-as-adds copied
			      :notice-ancestry ancestry
			      :output-format output-format))
		      ()))
      (setq revision (widget-create 'hare--svn-revision-choice))
      (insert ?\n)
      (setq properties (apply #'widget-create 'menu-choice
			      :value nil
			      :format "%t %[ Value Menu %]: %v"
			      :tag "Properties"
			      :help-echo "Define how to handle changes in properties"
			      '((const
				 :value nil
				 :format "%t\n%h"
				 :tag "Display"
				 :doc "Display differences in properties.")
				(const
				 :value ignore
				 :format "%t\n%h"
				 :tag "Ignore"
				 :doc "Ignore differences in properties.")
				(const
				 :value only
				 :format "%t\n%h"
				 :tag "Only"
				 :doc "Only operate on properties; don't compare file content."))))
      (insert ?\n)
      (setq whitespace (apply #'widget-create 'menu-choice
			      :value nil
			      :format "%t %[ Value Menu %]: %v"
			      :tag "Whitespace"
			      :help-echo "Define how to handle changes in whitespace"
			      '((const
				 :value nil
				 :format "%t\n%h"
				 :tag "Display"
				 :doc "Display differences in whitespace.")
				(const
				 :value "-b"
				 :format "%t\n%h"
				 :tag "Ignore Amount"
				 :doc "Ignore differences in amount of whitespace.
If enabled, ignore whitespace at the end of a line and consider all
other sequences of one or more whitespace characters within a line
to be equivalent.")
				(const
				 :value "-w"
				 :format "%t\n%h"
				 :tag "Ignore All"
				 :doc "Ignore all differences in whitespace.
If enabled, ignore differences even if one line has whitespace where
the other line has none."))))
      (insert ?\n)
      (setq newline (hare--create-svn-widget 'checkbox
		      :doc "Ignore differences in end of line style."))
      (insert ?\n)
      (setq added (hare--create-svn-widget 'checkbox
		    :doc "Don't display differences for added files.
If enabled, do not display differences for added files.  Otherwise,
an added file is displayed as if you had added all of its content
to an empty file."
		    :value t))
      (insert ?\n)
      (setq deleted (hare--create-svn-widget 'checkbox
		      :doc "Don't display differences for deleted files.
If enabled, do not display differences for deleted files.  Otherwise,
a deleted file is displayed as if you had deleted all of its content."
		      :value t))
      (insert ?\n)
      (setq copied (hare--create-svn-widget 'checkbox
		     :doc "Don't compare copied or moved files with their source files.
If enabled, treat copied or moved files like added files.  Otherwise,
a copied or moved file is compared against the file from which the
copy was created."))
      (insert ?\n)
      (setq ancestry (hare--create-svn-widget 'checkbox
		       :doc "Pay attention to ancestry when calculating differences.
If enabled, treat a file with identical content but different ancestry
as if it has been deleted and added again.  The default behavior is to
only compare the file content."))
      (insert ?\n)
      (setq output-format (apply #'widget-create 'menu-choice
				 :value nil
				 :format "%t %[ Value Menu %]: %v"
				 :tag "Output Format"
				 :help-echo "Define the output format"
				 '((const
				    :value nil
				    :format "%t\n%h"
				    :menu-tag "Standard"
				    :doc "Standard output format.")
				   (const
				    :value patch
				    :format "%t\n%h"
				    :menu-tag "Patch"
				    :doc "Patch compatible output format.")
				   (const
				    :value git
				    :format "%t\n%h"
				    :menu-tag "Git"
				    :doc "Git's extended output format.")
				   (const
				    :value summary
				    :format "%t\n%h"
				    :menu-tag "Summary"
				    :doc "Display only high-level status changes."))))
      (insert ?\n)
      (setq depth (hare--create-svn-widget 'depth :value 'infinity))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-log (targets &rest options)
  "Run the ‘svn log’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-process-window (buffer '(svn-log))
    (apply #'hare--svn buffer 0 targets "log"
	   (nconc (when-let ((revision (plist-get options :revision)))
		    (list "--revision" (hare--string revision t)))
		  (when-let ((change (plist-get options :change)))
		    (list "--change" (hare--string change t)))
		  (when (plist-get options :quiet)
		    (list "--quiet"))
		  (when (plist-get options :verbose)
		    (list "--verbose"))
		  (when (plist-get options :use-merge-history)
		    (list "--use-merge-history"))
		  (when (plist-get options :stop-on-copy)
		    (list "--stop-on-copy"))
		  (when-let ((limit (plist-get options :limit)))
		    (list "--limit" (hare--string limit t)))
		  (when-let ((search (plist-get options :search)))
		    ;; TODO: Handle multiple search patterns.
		    (list "--search" search))
		  ()))))

(defun hare-svn-log ()
  "Display information about the history of files and directories."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--form (targets revision limit quiet verbose merge copy)
	"Display information about the history of files and directories."
	(apply #'hare--svn-log targets
	       (nconc (cl-multiple-value-bind (first second third)
			  (cl-values-list revision)
			(cond ((and (eq first 'change) second)
			       (list :change second))
			      ((and (eq first 'revision) second)
			       (list :revision (if (null third)
						   (hare--string second t)
						 (format "%s:%s" second third))))))
		      (list :quiet quiet
			    :verbose verbose
			    :use-merge-history merge
			    :stop-on-copy copy)))
      (setq revision (widget-create 'hare--svn-revision-choice))
      (insert ?\n)
      (setq limit (hare--create-svn-widget 'limit))
      (insert ?\n)
      (setq quiet (hare--create-svn-widget 'checkbox
		    :doc "Don't display the commit log messages."))
      (insert ?\n)
      (setq verbose (hare--create-svn-widget 'checkbox
		      :doc "Display the affected files and directories, too."))
      (insert ?\n)
      (setq merge (hare--create-svn-widget 'checkbox
		    :doc "Display additional information from the merge history."))
      (insert ?\n)
      (setq copy (hare--create-svn-widget 'checkbox
		   :doc "Don't cross copies while traversing the history.
If enabled, the listing stops if a file or directory was added due to
a copy operation.  This can be useful for determining branch points."))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-status (targets &rest options)
  "Run the ‘svn status’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
  (hare--with-process-window (buffer '(svn-status))
    (apply #'hare--svn buffer 0 targets "status"
	   (nconc (when-let ((revision (plist-get options :revision)))
		    (list "--revision" (hare--string revision t)))
		  (when (plist-get options :no-ignore)
		    (list "--no-ignore"))
		  (when (plist-get options :ignore-externals)
		    (list "--ignore-externals"))
		  (when (plist-get options :quiet)
		    (list "--quiet"))
		  (when (plist-get options :show-updates)
		    (list "--show-updates"))
		  (when (plist-get options :verbose)
		    (list "--verbose"))
		  (when-let ((depth (plist-get options :depth)))
		    (list "--depth" (hare--string depth)))
		  ()))))

(defun hare-svn-status ()
  "Display the status of working copy files and directories."
  (interactive)
  (let ((paths (hare--svn-collect-paths)))
    (hare--form (targets depth revision quiet updates verbose no-ignore externals)
	"Display the status of working copy files and directories."
	(hare--svn-status targets
			  :depth depth
			  :revision revision
			  :quiet quiet
			  :show-updates updates
			  :verbose verbose
			  :no-ignore no-ignore
			  :ignore-externals externals)
      (setq revision (widget-create 'hare--svn-revision))
      (insert ?\n)
      (setq quiet (hare--create-svn-widget 'checkbox
		    :doc "Display only summary information about locally modified items."))
      (insert ?\n)
      (setq updates (hare--create-svn-widget 'checkbox
		      :doc "Display working copy revision and server out-of-date information."))
      (insert ?\n)
      (setq verbose (hare--create-svn-widget 'checkbox
		      :doc "Display full revision information on every item."))
      (insert ?\n)
      (setq no-ignore (hare--create-svn-widget 'checkbox
			:doc "Don't apply ignore rules to implicitly visited items.
Subversion uses ignore patterns to determine which items should be
skipped as part of a larger recursive operation.  If this option is
enabled, operate on all the files and directories present."))
      (insert ?\n)
      (setq externals (hare--create-svn-widget 'checkbox
			:doc "Ignore external definitions.
Don't operate on externals defined by ‘svn:externals’ properties."))
      (insert ?\n)
      (setq depth (hare--create-svn-widget 'depth :value 'infinity))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths))
      ())))

(defun hare--svn-cleanup (targets &rest options)
  "Run the ‘svn cleanup’ command.

First argument TARGETS are the targets of the command.  Value is
 either a list of file names or a HareSVN paths structure.
Remaining arguments OPTIONS are keyword arguments.

Return non-nil if the command succeeds."
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
	   "--include-externals"))
	(quiet
	 (when (plist-get options :quiet)
	   "--quiet")))
    (hare--with-process-window (buffer '(svn-cleanup))
      (hare--all-null
       ;; The actual clean up command.
       (unless (plist-get options :no-cleanup)
	 (not (apply #'hare--svn buffer 0 targets "cleanup"
		     (delq nil (list include-externals
				     quiet)))))
       ;; The alternative clean up command.
       (when (or remove-unversioned remove-ignored vacuum-pristines)
	 (not (apply #'hare--svn buffer 0 targets "cleanup"
		     (delq nil (list remove-unversioned
				     remove-ignored
				     vacuum-pristines
				     include-externals
				     quiet)))))))))

(defun hare-svn-cleanup ()
  "Recursively clean up the working copy."
  (interactive)
  (let ((paths (hare--svn-collect-paths
		:collect-parent t)))
    (hare--form (targets cleanup unversioned ignored vacuum externals)
	"Recursively clean up the working copy."
	(hare--svn-cleanup targets
			   :no-cleanup (not cleanup)
			   :remove-unversioned unversioned
			   :remove-ignored ignored
			   :vacuum-pristines vacuum
			   :include-externals externals)
      (setq cleanup (hare--create-svn-widget 'checkbox
		      :doc "Clean up working copy status.
Remove all write locks (shown as ‘L’ by the ‘svn status’ command) from
the working copy.  Usually, this is only necessary if a Subversion client
has crashed while using the working copy, leaving it in an unusable state."
		      :value t))
      (insert ?\n)
      (setq unversioned (hare--create-svn-widget 'checkbox
			  :doc "Remove unversioned files and directories."))
      (insert ?\n)
      (setq ignored (hare--create-svn-widget 'checkbox
		      :doc "Remove ignored files and directories."))
      (insert ?\n)
      (setq vacuum (hare--create-svn-widget 'checkbox
		     :doc "Remove unreferenced original files from ‘.svn’ directory."))
      (insert ?\n)
      (setq externals (hare--create-svn-widget 'checkbox
			:doc "Include external definitions.
Also operate on externals defined by ‘svn:externals’ properties."))
      (hare--form-horizontal-line)
      (setq targets (widget-create 'hare--paths-widget :value paths :hare-checked nil))
      ())))

(defconst hare--svn-menu
  (let ((menu (make-sparse-keymap "HareSVN")))
    (bindings--define-key menu [hare-svn-cleanup]
      '(menu-item "Clean up..." hare-svn-cleanup
		  :help "Recursively clean up the working copy"))
    (bindings--define-key menu [hare--svn-separator-3]
      menu-bar-separator)
    (bindings--define-key menu [hare-svn-status]
      '(menu-item "Status..." hare-svn-status
		  :help "Display the status of working copy files and directories"))
    (bindings--define-key menu [hare-svn-log]
      '(menu-item "Log..." hare-svn-log
		  :help "Display the history of files and directories"))
    (bindings--define-key menu [hare-svn-diff-revisions]
      '(menu-item "Diff..." hare-svn-diff-revisions
		  :help "Display differences between two revisions"))
    (bindings--define-key menu [hare-svn-diff]
      '(menu-item "Diff" hare-svn-diff
		  :help "Display local modifications in a working copy"))
    (bindings--define-key menu [hare--svn-separator-2]
      menu-bar-separator)
    (bindings--define-key menu [hare-svn-revert]
      '(menu-item "Revert..." hare-svn-revert
		  :help "Undo local modifications"))
    (bindings--define-key menu [hare-svn-delete]
      '(menu-item "Delete..." hare-svn-delete
		  :help "Remove files and directories from version control"))
    (bindings--define-key menu [hare-svn-add]
      '(menu-item "Add..." hare-svn-add
		  :help "Put files and directories under version control"))
    (bindings--define-key menu [hare--svn-separator-1]
      menu-bar-separator)
    (bindings--define-key menu [hare-svn-resolve]
      '(menu-item "Resolve..." hare-svn-resolve
		  :help "Resolve conflicts on working copy files or directories"))
    (bindings--define-key menu [hare-svn-update]
      '(menu-item "Update..." hare-svn-update
		  :help "Update your working copy"))
    (bindings--define-key menu [hare-svn-commit]
      '(menu-item "Commit..." hare-svn-commit
		  :help "Send changes from your working copy to the repository"))
    menu)
  "HareSVN menu for Subversion.")

(provide 'hare)

;;; hare.el ends here

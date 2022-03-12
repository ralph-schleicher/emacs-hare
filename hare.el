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
(require 'dired)

(defgroup hare nil
  "Hare is a TortoiseSVN clone for Dired buffers."
  :prefix "hare-"
  :group 'vc
  :group 'dired)

(defsubst hare--vc-state (file-name)
  "Return the Hare VC state of FILE-NAME.

Value is the VC state symbol as returned by the ‘vc-state’ function,
or ‘locked’ if the VC state indicates that the file is locked by some
other user."
  (let ((state (vc-state file-name)))
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
  "Alist of Hare properties associated with a VC state.
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
	    (setq ascent 'center)
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
      (insert "List of Hare version control states.\n\n")
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
      (setq truncate-lines t))))

(defun hare--dired-after-readin ()
  "Enrich the Dired buffer with Hare data."
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (while (not (eobp))
	(when (dired-move-to-filename)
	  (let* ((file (dired-get-filename nil t))
      		 (state (and file (hare--vc-state file))))
	    (if (hare--display-graphic-p)
		(hare--insert-image state)
	      (let ((prop (cdr (assq state hare--vc-state-alist)))
		    (mark (point)))
		(insert (cl-getf prop :flag))
		(put-text-property mark (point) 'help-echo (cl-getf prop :help))))
	    (insert " ")))
	(forward-line 1)))))

(with-eval-after-load "dired"
  (add-hook 'dired-after-readin-hook 'hare--dired-after-readin))

(provide 'hare)

;;; hare.el ends here

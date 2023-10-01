;;; hexl-note.el --- Annotate byte sequences in hexl-mode -*- lexical-binding: t -*-

;; Copyright (C) 2023 Nick OBrien

;; Author: Nick OBrien <nick4f42@proton.me>
;; Created: 2023
;; Homepage: https://github.com/nick4f42/hexl-note
;; Keywords: data
;; Package-Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `hexl-note-mode' allows annotating sequences of bytes in `hexl-mode'. See
;; `hexl-note-annotate' to annotate a sequence of bytes with a function of those
;; bytes, and see `hexl-note-annotate-number' to annotate bytes with their
;; integer or floating point representation.

;;; Code:

(require 'hexl)
(eval-when-compile (require 'cl-lib))

;;;; Customization

(defgroup hexl-note nil
  "Annotate byte sequences in `hexl-mode' buffers."
  :group 'data)

(defcustom hexl-note-padding 1
  "Horizontal padding spaces around annotation text.
This many horizontal spaces are ensured between any two
annotations."
  :group 'hexl-note
  :type 'natnum)

(defcustom hexl-note-arrow-string '(hexl-note-arrow ?< ?- ?> ?|)
  "Returns a string that denotes a range of bytes.
If this is a function, it should take the arguments LENGTH,
LEFT-TIP, and RIGHT-TIP. It should then return a string of LENGTH
that acts as the arrow. LEFT-TIP and RIGHT-TIP indicate whether
the left and right sides of the arrow should have arrow tips.

Otherwise, this variable is evaluated to get said function."
  :group 'hexl-note
  :type '(choice function sexp))

(defcustom hexl-note-od-program "od"
  "The POSIX od command used to unpack numbers from raw bytes."
  :group 'hexl-note
  :type 'string)

(defcustom hexl-note-number-types
  '((?i "signed integer" int nil)
    (?u "unsigned integer" uint nil)
    (?f "floating point" float nil))
  "List of number types for `hexl-note-annotate-number'.
Each element should be of the form (CHAR NAME TYPE SIZE). CHAR is
the character that selects the type. NAME is displayed as the
type's name. SIZE is the byte size of the number to annotate. If
SIZE is nil, the user is prompted for a size. TYPE and SIZE
correspond to the arguments in `hexl-note-unpack-bytes'."
  :group 'hexl-note
  :type '(repeat
	  (list (character :tag "Key to select")
		(string :tag "Name")
		(choice :tag "Type"
			(const :tag "Signed integer" int)
			(const :tag "Unsigned integer" uint)
			(const :tag "Floating point" float))
		(choice :tag "Byte size"
			natnum
			(const nil)))))

(defgroup hexl-note-faces nil
  "Faces used by hexl-note."
  :group 'hexl-note
  :group 'faces)

(defface hexl-note-arrow-face
  '((t (:inherit font-lock-builtin-face)))
  "Face used for byte sequence arrows."
  :group 'hexl-note-faces)

(defface hexl-note-selected-arrow-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for selected byte sequence arrows."
  :group 'hexl-note-faces)

(defface hexl-note-text-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for annotation text."
  :group 'hexl-note-faces)

(defvar-keymap hexl-note-mode-map
  :doc "Keymap used by `hexl-note-mode'."
  "C-c C-n" #'hexl-note-next-annot
  "C-c C-p" #'hexl-note-previous-annot
  "C-c C-d" #'hexl-note-delete-selected
  "C-c C-a" #'hexl-note-annotate
  "C-c C-i" #'hexl-note-annotate-number)

;;;; Types

(cl-defstruct (hexl-note--annot
	       (:constructor hexl-note--make-annot)
	       (:copier nil))
  "An annotation of a particular sequence of bytes."
  (start-addr
   nil
   :read-only t
   :type natnum
   :documentation "Inclusive starting address of the annotation.")
  (end-addr
   nil
   :read-only t
   :type natnum
   :documentation "Exclusive ending address of the annotation.")
  (label
   nil
   :read-only t
   :type function
   :documentation "Function that returns the label string for an annotation.")
  (lines
   nil
   :type list
   :documentation "List of `hexl-note--line' for each spanned line.")
  (label-line
   nil
   :type hexl-note--line
   :documentation "The `hexl-note--line' that has the label."))

(cl-defstruct (hexl-note--figure
	       (:constructor hexl-note--make-figure)
	       (:copier nil))
  "The annotation text and where it should be displayed."
  (text-rows
   nil
   :type vector
   :documentation "Vector of strings for each row of text.")
  (x
   nil
   :type natnum
   :documentation "The column of the left-most text.")
  (y
   nil
   :type natnum
   :documentation "The row of the upper-most text relative to the parent line."))

(cl-defstruct (hexl-note--line
	       (:constructor hexl-note--make-line)
	       (:copier nil))
  "The annotations for a single line of the `hexl-mode' buffer."
  (num
   nil
   :read-only t
   :type natnum
   :documentation "The line number in the buffer.")
  (annot-alist
   nil
   :type list
   :documentation "Alist mapping `hexl-note--annot's to `hexl-note--figure's.")
  (overlay
   nil
   :read-only t
   :type overlay
   :documentation "The overlay displayed after the line.")
  (rows
   nil
   :type vector
   :documentation "Vector of strings for each row of text to display."))

;;;; Internal Variables

(defvar-local hexl-note--lines nil
  "Hash table mapping line numbers `hexl-note--line's.")

(defvar-local hexl-note--selected-annot nil
  "The currently selected `hexl-note--annot'.")

;;;; Modes

;;;###autoload
(define-minor-mode hexl-note-mode
  "Toggle annotations in the `hexl-mode' buffer."
  :global nil
  :keymap hexl-note-mode-map
  (if hexl-note-mode
      (progn
	(unless (eq major-mode 'hexl-mode)
	  (setq hexl-note-mode nil)
	  (user-error "Must be in a `hexl-mode' buffer to use `hexl-note-mode'"))
	(unless hexl-note--lines
	  (setq hexl-note--lines (make-hash-table)))
	(add-hook 'post-command-hook #'hexl-note--post-command nil t)
	(add-hook 'after-change-functions #'hexl-note--after-change nil t)
	(add-hook 'change-major-mode-hook #'hexl-note--remove-overlays nil t)
	(advice-add 'hexl-save-buffer :after #'hexl-note--readjust-overlays))

    (hexl-note-delete-all)
    (setq hexl-note--lines nil)
    (remove-hook 'post-command-hook #'hexl-note--post-command t)
    (remove-hook 'after-change-functions #'hexl-note--after-change t)
    (remove-hook 'change-major-mode-hook #'hexl-note--remove-overlays t)))

;;;; Annotation Addition/Removal

(defun hexl-note-annotate (start end label)
  "Add an annotation from START (inclusive) to END (exclusive).
LABEL is a function of START and BYTES that returns the
annotation string. Interactively, START is the current address,
END is START plus the prefix numeric value or a number read from
the minibuffer."
  (interactive
   (let* ((start (hexl-current-address))
	  (size (if current-prefix-arg
		    (prefix-numeric-value current-prefix-arg)
		  (floor (read-number "Count: ")))))
     (list start
	   (+ start size)
	   (eval `(lambda (start bytes)
		    (format "%s" ,(read-minibuffer "Function of (start bytes): ")))))))
  (when (> (1- end) hexl-max-address)
    (user-error "Annotation extends past max address"))
  (when (< end start)
    (user-error "Annotation END must not be less than START"))
  (let* ((annot (hexl-note--make-annot
		 :start-addr start :end-addr end :label label))
	 ;; Column indices, x
	 (first-x-min (hexl-note--address-to-col start)) ; x of first byte
	 (mid-x-min (hexl-note--min-col)) ; min x between first and last lines
	 (mid-x-max (hexl-note--max-col)) ; max x between first and last lines
	 (last-x-max (1+ (hexl-note--address-to-col (1- end)))) ; x of last byte
	 ;; Line numbers
	 (min-line (hexl-note--address-to-line start)) ; line of first byte
	 (max-line (hexl-note--address-to-line (1- end))) ; line of last byte
	 (mid-line (+ min-line (/ (- max-line min-line) 2))) ; line for label
	 (line max-line)
	 lines)

    (while (>= line min-line)
      (let* ((end-left (= line min-line))
	     (end-right (= line max-line))
	     (x-min (if end-left first-x-min mid-x-min))
	     (x-max (if end-right last-x-max mid-x-max))
	     (length (1+ (- x-max x-min)))
	     (text (if (= line mid-line)
		       (funcall label start (hexl-note--bytes start end))))
	     (figure (hexl-note--make-figure
		      :text-rows (hexl-note--annot-rows
				  text length end-left end-right)
		      :x x-min))
	     (note-line (hexl-note--get-line line))
	     (annot-alist (hexl-note--line-annot-alist note-line)))

	(push (cons annot figure) annot-alist)
	(setf (hexl-note--line-annot-alist note-line)
	      (sort annot-alist #'hexl-note--annot-alist-sort-p))

	(hexl-note--update-line note-line)

	(push note-line lines)
	(when (= line mid-line)
	  (setf (hexl-note--annot-label-line annot) note-line)))

      (setq line (1- line)))

    (setf (hexl-note--annot-lines annot) lines)))

(defun hexl-note-delete-all ()
  "Delete all annotations in the buffer."
  (interactive)
  (hexl-note--remove-overlays)
  (clrhash hexl-note--lines)
  (setq hexl-note--selected-annot nil))

(defun hexl-note--remove-overlays ()
  (remove-overlays nil nil 'hexl-note t))

(defun hexl-note-delete-selected (&optional count)
  "\\<hexl-note-mode-map>Delete the selected annotation.
With COUNT, delete a total of COUNT annotations. Use
\\[hexl-note-next-annot] or \\[hexl-note-previous-annot] to select an annotation."
  (interactive "p")
  (or count (setq count 1))
  (unless hexl-note--selected-annot
    (user-error (substitute-command-keys "No annotation selected. Use \\[hexl-note-next-annot] or \\[hexl-note-previous-annot] to select an annotation.")))
  (dotimes (i count)
    (let ((current hexl-note--selected-annot)
	  (last (= i (1- count))))
      (hexl-note-next-annot 1 last)
      (hexl-note-delete-annotation current))))

(defun hexl-note-delete-annotation (annot)
  "Remove the `hexl-note--annot' annotation ANNOT."
  (when (eq annot hexl-note--selected-annot)
    (setq hexl-note--selected-annot nil))
  (dolist (note-line (hexl-note--annot-lines annot))
    (let ((annot-alist (hexl-note--line-annot-alist note-line)))
      (if (and (length= annot-alist 1)
	       (eq (caar annot-alist) annot))
	  (progn
	    (delete-overlay (hexl-note--line-overlay note-line))
	    (remhash (hexl-note--line-num note-line) hexl-note--lines))
	(setf (hexl-note--line-annot-alist note-line)
	      (assq-delete-all annot annot-alist))
	(hexl-note--update-line note-line)))))

(defun hexl-note--annot-alist-sort-p (cons1 cons2)
  "Orders members of `hexl-note--line-annot-alist'."
  (let ((n1 (hexl-note--annot-sort-nums (car cons1)))
	(n2 (hexl-note--annot-sort-nums (car cons2))))
    (catch 'return
      (while (and n1 n2)
	(when (/= (car n1) (car n2))
	  (throw 'return (< (car n1) (car n2))))
	(pop n1)
	(pop n2)))))

(defun hexl-note--annot-sort-nums (annot)
  "List of numbers to sort by in `hexl-note--annot-alist-sort-p'."
  (list
   ;; Order left to right based on starting address
   (hexl-note--annot-start-addr annot)
   ;; For the same starting address, next sort by ending address
   (hexl-note--annot-end-addr annot)
   ;; Finally, hash the annotation label for consistent ordering
   (sxhash (hexl-note--annot-label annot))))

(defun hexl-note--get-line (line)
  "Get or create the `hexl-note--line' at line number LINE."
  (let ((note-line (gethash line hexl-note--lines)))
    (unless note-line
      (let* ((pos (hexl-note--eol-pos line))
	     (overlay (make-overlay pos pos nil nil t)))
	(overlay-put overlay 'hexl-note t)
	(setq note-line (hexl-note--make-line :num line :overlay overlay))
	(puthash line note-line hexl-note--lines)))
    note-line))

;;;; Annotation Updating

(defun hexl-note--after-change (start end prev-length)
  "Function for `after-change-functions'."
  (when (zerop prev-length)
    (pcase-let
	((`(,first-addr . ,first-valid) (hexl-note--pos-to-address start))
	 (`(,last-addr . ,last-valid) (hexl-note--pos-to-address (1- end))))
      ;; Don't update if the change is in the ASCII region
      (unless (and (= first-addr last-addr) (not first-valid) (not last-valid))
	(hexl-note--update-annots first-addr (1+ last-addr))))))

(defun hexl-note--post-command ()
  (unless (memq this-command
		'( hexl-note-next-annot hexl-note-previous-annot
		   hexl-note-delete-selected))
    (hexl-note-unselect-annot)))

(defun hexl-note--update-annots (start end)
  "Re-call the annotation functions between address START and END."
  (let* ((min-line (hexl-note--address-to-line start))
	 (max-line (hexl-note--address-to-line (1- end)))
	 (line min-line))
    (while (<= line max-line)
      (when-let ((note-line (gethash line hexl-note--lines)))
	(let (need-update)
	  (pcase-dolist
	      (`(,annot . ,_) (hexl-note--line-annot-alist note-line))
	    (let ((annot-start (hexl-note--annot-start-addr annot))
		  (annot-end (hexl-note--annot-end-addr annot)))
	      ;; Check if the annotation is within START and END
	      (when (cond ((< start annot-start)
			   (< annot-start end))
			  ((< start annot-end)
			   t))
		(setq need-update t)
		(hexl-note--update-annot annot t))))
	  (when need-update
	    (hexl-note--update-line note-line))))

      (setq line (1+ line)))))

(defun hexl-note--update-annot (annot &optional no-update-line)
  "Update the annotation text.
With NO-UPDATE-LINE, do not update the `hexl-note--line' to which
the annotation label belongs."
  (let* ((start-addr (hexl-note--annot-start-addr annot))
	 (end-addr (hexl-note--annot-end-addr annot))
	 (label-line (hexl-note--annot-label-line annot))
	 (annot-alist (hexl-note--line-annot-alist label-line))
	 (figure (cdr (assq annot annot-alist)))
	 (text (funcall (hexl-note--annot-label annot)
			start-addr (hexl-note--bytes start-addr end-addr))))

    (setf (hexl-note--figure-text-rows figure)
	  (vconcat (list (aref (hexl-note--figure-text-rows figure) 0))
		   (hexl-note--text-rows text)))

    (unless no-update-line
      (hexl-note--update-line label-line))))

(defun hexl-note--update-line (note-line)
  "Re-fit the annotations on the given line."
  (let ((annot-alist (hexl-note--line-annot-alist note-line))
	(overlay (hexl-note--line-overlay note-line))
	(rows (vector)))

    (pcase-dolist (`(,_ . ,figure) annot-alist)
      (let* ((src-rows (hexl-note--figure-text-rows figure))
	     (x (hexl-note--figure-x figure))
	     (fit (hexl-note--fit-rows rows 0 x src-rows))
	     (y (cdr fit)))
	(setq rows (car fit))
	(setf (hexl-note--figure-y figure) y)))

    (setf (hexl-note--line-rows note-line) rows)

    (overlay-put
     overlay 'after-string
     (hexl-note--rows-to-string
      (if-let ((figure (cdr (assq hexl-note--selected-annot annot-alist))))
	  (hexl-note--selected-rows rows figure)
	rows)))))

(defun hexl-note--readjust-overlays (&rest _)
  "Move the overlays back to the correct position."
  (maphash
   (lambda (line note-line)
     (let ((overlay (hexl-note--line-overlay note-line))
	   (pos (hexl-note--eol-pos line)))
       (move-overlay overlay pos pos)))
   hexl-note--lines))

;;;; Annotation Selecting

(defun hexl-note-next-annot (&optional arg noerror)
  "Select and move point to the next annotation.
With ARG, do it ARG times. If there are no more annotations, an
error will be signaled unless NOERROR."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((backward (< arg 0)))
    (dotimes (_ (abs arg))
      (hexl-note--step-annot backward noerror))))

(defun hexl-note-previous-annot (&optional arg noerror)
  "Select and move point to the previous annotation.
With ARG, do it ARG times. If there are no more annotations, an
error will be signaled unless NOERROR."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((backward (> arg 0)))
    (dotimes (_ (abs arg))
      (hexl-note--step-annot backward noerror))))

(defun hexl-note-unselect-annot ()
  "Unselect the currently selected annotation."
  (interactive)
  (when hexl-note--selected-annot
    (hexl-note--unselect-annot hexl-note--selected-annot)
    (setq hexl-note--selected-annot nil)))

(defun hexl-note--selected-line (annot)
  "The line that ANNOT belongs to for selecting annotations."
  (car (hexl-note--annot-lines annot)))

(defun hexl-note--step-annot (backward &optional noerror)
  (let ((annot
	 (if hexl-note--selected-annot
	     ;; Find the first annotation past the selected one
	     (let (past)
	       (hexl-note--next-annot
		(lambda (annot note-line)
		  (prog1 (and past (eq note-line (hexl-note--selected-line annot)))
		    (when (eq hexl-note--selected-annot annot)
		      (setq past t))))
		(hexl-note--line-num
		 (hexl-note--selected-line hexl-note--selected-annot))
		backward))

	   ;; Find the first annotation past point
	   (let ((addr (hexl-current-address)))
	     (hexl-note--next-annot
	      (if backward
		  (lambda (annot note-line)
		    (and (>= addr (hexl-note--annot-start-addr annot))
			 (eq note-line (hexl-note--selected-line annot))))
		(lambda (annot note-line)
		  (and (<= addr (hexl-note--annot-start-addr annot))
		       (eq note-line (hexl-note--selected-line annot)))))
	      (hexl-note--address-to-line addr)
	      backward)))))

    (if (not annot)
	(unless noerror
	  (user-error "No more annotations in buffer"))

      (when hexl-note--selected-annot
	(hexl-note--unselect-annot hexl-note--selected-annot))
      (setq hexl-note--selected-annot annot)

      (hexl-note--select-annot annot)
      (hexl-goto-address (hexl-note--annot-start-addr annot)))))

(defun hexl-note--next-annot (pred line &optional backward)
  (let ((reorder (if backward #'reverse #'identity))
	(step (if backward -1 1))
	(num line))
    (catch 'return
      (while t
	(if-let ((note-line (hexl-note--next-note-line num backward)))
	    (progn
	      (pcase-dolist
		  (`(,annot . ,_)
		   (funcall reorder (hexl-note--line-annot-alist note-line)))
		(when (funcall pred annot note-line)
		  (throw 'return annot)))
	      (setq num (+ step (hexl-note--line-num note-line))))
	  (throw 'return nil))))))

(defun hexl-note--next-note-line (line &optional backward)
  (or (gethash line hexl-note--lines)
      (let ((order (if backward #'> #'<))
	    (closer (if backward #'max #'min))
	    next)
	(dolist (n (hash-table-keys hexl-note--lines))
	  (when (funcall order line n)
	    (setq next (if next (funcall closer next n) n))))
	(gethash next hexl-note--lines))))

(defun hexl-note--select-annot (annot)
  "Display ANNOT as selected."
  (dolist (note-line (hexl-note--annot-lines annot))
    (let* ((overlay (hexl-note--line-overlay note-line))
	   (rows (hexl-note--line-rows note-line))
	   (annot-alist (hexl-note--line-annot-alist note-line))
	   (figure (cdr (assq annot annot-alist)))
	   (selected-rows (hexl-note--selected-rows rows figure)))
      (overlay-put overlay 'after-string
		   (hexl-note--rows-to-string selected-rows)))))

(defun hexl-note--unselect-annot (annot)
  "Remove selected display from ANNOT."
  (dolist (note-line (hexl-note--annot-lines annot))
    (let* ((overlay (hexl-note--line-overlay note-line))
	   (rows (hexl-note--line-rows note-line)))
      (overlay-put overlay 'after-string
		   (hexl-note--rows-to-string rows)))))

;;;; Annotation Drawing

;; Drawing of annotations is done by storing vectors of strings for each row.
;; Each annotation has their own rows for their arrow and text, and each
;; annotation is combined into a single vector of rows like a bitmap. Characters
;; in the string that aren't part of the annotation are set to 0 so that we can
;; check and prevent annotations from overlapping.

(defun hexl-note--rows-to-string (rows)
  (apply
   #'concat
   (mapcan (lambda (s)
	     (list "\n" (subst-char-in-string 0 ?\s s t)))
	   rows)))

(defun hexl-note--selected-rows (rows figure)
  "Return ROWS where the arrow in FIGURE has a selected face."
  (let* ((x (hexl-note--figure-x figure))
	 (y (hexl-note--figure-y figure))
	 (length (length (aref (hexl-note--figure-text-rows figure) 0)))
	 (row (copy-sequence (aref rows y)))
	 (out-rows (copy-sequence rows)))
    ;; Only add the face to the first row of the figure, i.e. the arrow
    (add-face-text-property
     x (+ x length) 'hexl-note-selected-arrow-face nil row)
    (aset out-rows y row)
    out-rows))

(defun hexl-note--annot-rows (string arrow-length &optional left-tip right-tip)
  "Returns a length ARROW-LENGTH arrow with the text STRING underneath."
  (let ((arrow
	 (funcall (if (functionp hexl-note-arrow-string)
		      hexl-note-arrow-string
		    (eval hexl-note-arrow-string))
		  arrow-length left-tip right-tip)))
    (add-face-text-property 0 (length arrow) 'hexl-note-arrow-face t arrow)
    (vconcat (list arrow)
	     (if string (hexl-note--text-rows string)))))

(defun hexl-note--text-rows (string)
  (add-face-text-property
   0 (length string) 'hexl-note-text-face t string)
  (split-string string "\n"))

(defun hexl-note-arrow (left center right &optional single)
  "Returns a function for `hexl-note-arrow-string'."
  (or single (setq single ?|))
  (lambda (length left-tip right-tip)
    (cond ((= length 0) "")
	  ((= length 1) (string single))
	  (t
	   (let ((arrow (make-string length center)))
	     (when left-tip (aset arrow 0 left))
	     (when right-tip (aset arrow (1- length) right))
	     arrow)))))

(defun hexl-note--fit-rows (out-rows y x src-rows)
  "Like `hexl-note--draw-rows', but adjusts Y to not intersect text.
Returns (ROWS . Y-FIT) where Y-FIT is the adjusted Y position."
  (let ((x-pad
	 (lambda (y)
	   ;; Don't pad the arrow in the first row (y=0)
	   (if (zerop y) 0 hexl-note-padding))))
    (catch 'complete
      (dotimes (_ 1000)
	(unless (hexl-note--rows-intersect-p out-rows y x src-rows x-pad)
	  (throw 'complete
		 (cons (hexl-note--draw-rows out-rows y x src-rows)
		       y)))
	(setq y (1+ y)))
      (error "Could not find a row to draw"))))

(defun hexl-note--rows-intersect-p (out-rows y x src-rows &optional x-pad)
  "Whether SRC-ROWS would overwrite non-null characters in OUT-ROWS.
Also checks an additional (funcall x-pad i) characters to the
left and right of each row i in SRC-ROWS."
  (setq x-pad (or x-pad (lambda (_) 0)))
  (let ((y-max (min (length out-rows) (+ y (length src-rows))))
	x-min x-max pad
	x-src y-src x-out y-out
	out-row src-row)

    (catch 'intersect-found
      (setq y-src 0
	    y-out y)
      (while (< y-out y-max)
	(setq src-row (aref src-rows (- y-out y))
	      out-row (aref out-rows y-out)
	      pad (funcall x-pad y-src)
	      x-min (max 0 (- x pad))
	      x-max (min (length out-row) (+ x (length src-row) pad))
	      x-src (- x-min x)
	      x-out x-min)

	(while (< x-out x-max)
	  (when (and
		 ;; Check nearest character in src-row
		 (/= 0 (aref src-row
			     (max 0 (min (1- (length src-row)) x-src))))
		 ;; Check character in out-row
		 (/= 0 (aref out-row x-out)))
	    (throw 'intersect-found t))
	  (setq x-src (1+ x-src)
		x-out (1+ x-out)))

	(setq y-src (1+ y-src)
	      y-out (1+ y-out)))
      nil)))

(defun hexl-note--draw-rows (out-rows y x src-rows)
  "Overwrite OUT-ROWS starting at index X in row Y with SRC-ROWS.
Returns the result, which may reuse OUT-ROWS."
  (let* ((out-count (length out-rows))
	 (src-count (length src-rows))
	 (extra-count (- (+ y src-count) out-count))
	 src-idx)

    (when (> extra-count 0)
      (setq out-rows
	    (vconcat out-rows
		     (make-vector extra-count ""))))

    (setq src-idx 0)
    (seq-doseq (src-row src-rows)
      (let* ((out-idx (+ y src-idx))
	     (out-row (aref out-rows out-idx)))
	(aset out-rows
	      out-idx
	      (hexl-note--draw-row out-row x src-row)))
      (setq src-idx (1+ src-idx)))

    out-rows))

(defun hexl-note--draw-row (out-row x src-row)
  "Overwrite OUT-ROW at index X with SRC-ROW.
Returns the result, which may reuse OUT-ROW."
  (let* ((out-length (length out-row))
	 (src-length (length src-row))
	 (extra-length (- (+ x src-length) out-length)))
    (when (> extra-length 0)
      (setq out-row (concat out-row (make-string extra-length 0))))
    ;; Store substring with properties intact
    (setf (substring out-row x (+ x src-length)) src-row)
    out-row))

;;;; Hexl Utils

(defun hexl-note--bytes (start end)
  "Return the string of bytes from START (inclusive) to END (exclusive)."
  (when (> (1- end) hexl-max-address)
    (error "Address greater than max address"))
  (let ((addr start)
	(bytes (make-string (- end start) 0)))
    (while (< addr end)
      (let* ((pos (hexl-address-to-marker addr))
	     (hexstr (buffer-substring-no-properties pos (+ pos 2)))
	     (num (string-to-number hexstr 16)))
	(aset bytes (- addr start) num)
	(setq addr (1+ addr))))
    bytes))

(defun hexl-note--eol-pos (line)
  "Buffer position of the end of line LINE."
  (min (buffer-size) (* (hexl-line-displen) line)))

(defun hexl-note--address-to-line (address)
  "The line number of the given hexl ADDRESS."
  (1+ (/ address 16)))

(defun hexl-note--address-to-col (address)
  "The column number of the given hexl ADDRESS."
  (let ((n (* 2 (% address 16))))
    (+ 10
       n
       (/ n (/ hexl-bits 4)))))

(defun hexl-note--pos-to-address (pos)
  "The buffer position of a hexl address.
Returns (ADDR . VALID) where ADDR is the largest address before
or are POS, and VALID is whether the position is at an address
and not in the margins."
  (let* ((displen (hexl-line-displen))
	 (i (1- pos))
	 (line (/ i displen))
	 (col (- (% i displen) 10))
	 (group-len (1+ (/ hexl-bits 4)))
	 (group-bytes (/ hexl-bits 8))
	 (line-addr (+ (* group-bytes (/ col group-len))
		       (/ (% col group-len) 2))))
    (cons (+ (* 16 line) (min 15 (max 0 line-addr)))
	  (<= 0 line-addr 15))))

(defun hexl-note--min-col ()
  "Minimum column where bytes are displayed."
  (hexl-note--address-to-col 0))

(defun hexl-note--max-col ()
  "Maximum column where bytes are displayed."
  (1+ (hexl-note--address-to-col 15)))

;;;; Annotators

(defun hexl-note-annotate-number (type size &optional endian start count)
  "Annotate COUNT numbers starting from address START.
A numeric prefix sets COUNT. TYPE, SIZE, and ENDIAN are prompted
for using `hexl-note-number-types'."
  (interactive
   (append (hexl-note--read-number-type)
	   (list
	    (hexl-current-address)
	    (prefix-numeric-value current-prefix-arg))))
  (or start (setq start (hexl-current-address)))
  (or count (setq count 1))
  (let ((addr start)
	(end (+ start (* size count)))
	(label (hexl-note-number-label type size endian)))
    (while (< addr end)
      (hexl-note-annotate addr (setq addr (+ addr size)) label)
      (when (<= addr hexl-max-address)
	(hexl-goto-address addr)))))

(defun hexl-note-number-label (type size &optional endian)
  "Create a number label function suitable for `hexl-note-annotate'."
  (lambda (_start bytes)
    (concat (alist-get
	     endian '((little-endian . "LE:")
		      (big-endian . "BE:")))
	    (car (hexl-note-unpack-bytes type size bytes endian)))))

(defun hexl-note-unpack-bytes (type size bytes &optional endian)
  "Unpack numbers of TYPE and SIZE from byte string BYTES.
Returns a list of strings for each number. The length of BYTES
must be divisible by SIZE. TYPE must be one of `int', `uint', and
`float'. ENDIAN must be one of `little-endian', `big-endian', or
nil to use od's default."
  (unless (zerop (% (length bytes) size))
    (error "Length of BYTES must be divisible by SIZE"))
  (let ((fmt
	 (cond ((eq type 'int) (format "d%d" size))
	       ((eq type 'float) (format "f%d" size))
	       ((eq type 'uint) (format "u%d" size))
	       (t (error "Invalid TYPE %S" type))))
	(endian
	 (cond ((not endian) nil)
	       ((eq endian 'little-endian) "little")
	       ((eq endian 'big-endian) "big")
	       (t (error "Invalid ENDIAN %S" endian)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert bytes)

      (unless (zerop
	       (apply #'call-process-region
		      (point-min) (point-max) hexl-note-od-program t t nil
		      (concat "-t" fmt)
		      `( "-An"
			 ,@(if endian
			       (list (concat "--endian=" endian))))))
	(error "Error in od process"))

      (split-string (buffer-string) "[ \n\r]+" t))))

(defun hexl-note--read-number-type ()
  (pcase-let*
      ((choice
	(car (read-multiple-choice
	      "Type: "
	      (mapcar (pcase-lambda (`(,char ,desc . ,_))
			(list char desc))
		      hexl-note-number-types) )))
       (`(,_ ,_ ,type ,size) (assq choice hexl-note-number-types))
       (size (if (integerp size)
		 size
	       (floor (read-number "Size: "))))
       (endian
	(alist-get (car (read-multiple-choice
			 "Endian: "
			 '((?l "little-endian")
			   (?b "big-endian")
			   (?\s "default"))))
		   '((?l . little-endian) (?b . big-endian)))))
    (list type size endian)))

(defun hexl-note--number-at-address (type size endian start)
  (funcall
   (hexl-note-number-label type size endian)
   start (hexl-note--bytes start (+ start size))))

(provide 'hexl-note)
;;; hexl-note.el ends here

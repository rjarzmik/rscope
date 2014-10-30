;;; rscope.el --- Reborn cscope interface for emacs
;;
;; Copyright(c) 2014 Robert Jarzmik <robert.jarzmik@free.fr>
;; Authors: Robert Jarzmik <robert.jarzmik@free.fr>
;; Keywords: code
;; Licence: GPLv2
;; Version 0.1
;;
;; Heavily inspired by ascope, relies on :
;;   - pre-launched cscope processes
;;   - auto finding of the best suited cscope process (based on file directory)
;;   - auto finding cscope databases, customizable
;;   - has navigation and preview capability in rscope result buffer

;; Usage:
;;   load this script using (require 'rscope) in you .emacs
;;   M-x rscope-init load one of the cscope databases.
;;     This command must be issue prior to issue any other command below, the
;;     directory fed to this command must be the directory holding the cscope.out
;;     file.
;;     Alternatively, if a cscope.out exists in the directory tree, don't bother
;;     doing a rscope-init command, it will work out automatically.
;;   M-x find-file "myfile.c"
;;   M-x rscope-find-this-symbol
;;     Find a symbol from the database.

;; Result buffer navigation (*Result*)
;;   Normal keystrokes :
;;     Use "n" to navigate to next entry in results
;;     Use "p" to navigate to previsous entry in results
;;     Use ENTER bury result buffer and switch to the previewed entry
;;     Use "q" to bury result buffer and switch to other window.
;;
;;   Advanced keystrokes:
;;     Use SPACE to preview an entry
;;     Use Ctr-ENTER to bury result buffer, and replace it by the selected entry
;;     Use number <N> to develop all subtrees up to Nth level (0 => expand all)
;;     Use "+" to expand a collapsed tree
;;     Use "-" to collapse a tree

;; Available global commands (bound by default to C-c s [gstCcia])
;;
;; M-x rscope-find-global-definition
;; M-x rscope-find-this-symbol
;; M-x rscope-find-this-text-string
;; M-x rscope-find-functions-calling-this-function
;; M-x rscope-find-called-functions
;; M-x rscope-find-files-including-file
;; M-x rscope-all-symbol-assignments
;; M-x rscope-pop-mark

;; Advanced users guide :
;;  - pay attention to cusotmizable variables
;;  - don't do anymore any rscope-init command, implement a hook in
;;    rscope-autoinit-cscope-dir-hooks
;;      (see rscope-autoinit-path-upwards-cscope_out)

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://www.fsf.org/copyleft/gpl.html

(require 'outline nil t)
(require 'tramp)

(defgroup rscope nil
  "Cscope interface for (X)Emacs.
Using cscope, you can easily search for where symbols are used and defined.
It is designed to answer questions like:

Where is this variable used?
What is the value of this preprocessor symbol?
Where is this function in the source files?
What functions call this function?
What functions are called by this function?
Where does the message \"out of space\" come from?
Where is this source file in the directory structure?
What files include this header file?
"
  :prefix "rscope-"
  :group 'tools)

(defcustom rscope-allow-arrow-overlays t
  "*If non-nil, use an arrow overlay to show target lines.

The arrow overlay is removed when other cscope functions are used.
Note that the arrow overlay is not an actual part of the text, and can
be removed by quitting the cscope buffer."
  :type 'boolean
  :group 'rscope)

(defcustom rscope-hierarchies-shorten-filename t
  "*If non-nil, replace relative path names with the stripped of path, just
as would have been gotten by using unix basename."
  :type 'boolean
  :group 'rscope
)

(defcustom rscope-overlay-arrow-string "=>"
  "*The overlay string to use when displaying arrow overlays."
  :type 'string
  :group 'rscope)

(defface rscope-file-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face used to highlight file name in the *rscope* buffer."
  :group 'cscope)

(defface rscope-function-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight function name in the *rscope* buffer."
  :group 'rscope)


(defface rscope-line-number-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face used to highlight line number in the *rscope* buffer."
  :group 'rscope)


(defface rscope-line-face
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *rscope* buffer."
  :group 'rscope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst rscope-separator-line
  "-------------------------------------------------------------------------------\n"
  "Line of text to use as a visual separator.
Must end with a newline.")

(defvar rscope-action-message nil "The message about what action is taken")

(defvar rscope-list-entry-keymap nil
  "The keymap used in the *Result* buffer which lists search results.")
(if rscope-list-entry-keymap
    nil
  (setq rscope-list-entry-keymap (make-keymap))
  (suppress-keymap rscope-list-entry-keymap)
  (define-key rscope-list-entry-keymap "n" 'rscope-next-symbol)
  (define-key rscope-list-entry-keymap "p" 'rscope-prev-symbol)
  (define-key rscope-list-entry-keymap "q" 'rscope-close-results)
  (define-key rscope-list-entry-keymap " " 'rscope-preview-entry-other-window)
  (define-key rscope-list-entry-keymap (kbd "RET") 'rscope-select-entry-other-window)
  (define-key rscope-list-entry-keymap (kbd "S-<return>") 'rscope-select-entry-current-window)
  (define-key rscope-list-entry-keymap "R" 'rscope-regenerate-database)
  (when (featurep 'outline)
    (define-key rscope-list-entry-keymap "0" (lambda() (interactive) (show-all)))
    (define-key rscope-list-entry-keymap "1" (lambda() (interactive) (hide-sublevels 1)))
    (define-key rscope-list-entry-keymap "2" (lambda() (interactive) (hide-sublevels 2)))
    (define-key rscope-list-entry-keymap "3" (lambda() (interactive) (hide-sublevels 3)))
    (define-key rscope-list-entry-keymap "4" (lambda() (interactive) (hide-sublevels 4)))
    (define-key rscope-list-entry-keymap "-" (function hide-subtree))
    (define-key rscope-list-entry-keymap "+" (function show-subtree)))
  )

(defvar rscope-autoinit-cscope-dir-hooks nil
  "*Hooks run to perform automatic rcscope-init calls.
These hooks should take a buffer as input, and return a string containing
a directory with a cscope.out file, or nil if they can't find a directory.
The first hook returning a non nil value wins.")

(defvar rscope-list-entry-hook nil
  "*Hook run after rscope-list-entry-mode entered.")

(defvar rscope-output-buffer-name "*Result*"
  "The name of the cscope output buffer.")

(defvar rscope-marker-ring-length 30 )

(defvar rscope-marker-ring (make-ring rscope-marker-ring-length))

(defvar rscope:map nil
  "The rscope keymap.")
(unless rscope:map
    (define-prefix-command 'rscope:map)
    ;; The following line corresponds to be beginning of the "Cscope" menu.
    (define-key 'rscope:map "s" 'rscope-find-this-symbol)
    (define-key 'rscope:map "d" 'rscope-find-global-definition)
    (define-key 'rscope:map "g" 'rscope-find-global-definition)
    (define-key 'rscope:map "c" 'rscope-find-functions-calling-this-function)
    (define-key 'rscope:map "C" 'rscope-find-called-functions)
    (define-key 'rscope:map "t" 'rscope-find-this-text-string)
    (define-key 'rscope:map "i" 'rscope-find-files-including-file)
    (define-key 'rscope:map "h" 'rscope-find-calling-hierarchy)
    )

(defvar preview-buffers)
(defvar preview-already-opened-buffers)
(defvar rscope-level)
(defvar rscope-auto-open)
(defvar proc-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level user usable functions (init + queries)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-init (dir)
  (interactive "DCscope Initial Directory: ")
  (let* ((dir (expand-file-name dir))
	 (buffer-name (format "*rscope-%s*" dir))
	 (rscope-buffer (get-buffer-create buffer-name))
	 process)
    (with-current-buffer rscope-buffer
      (if (get-buffer-process buffer-name)
	  (kill-process (get-buffer-process buffer-name)))
      (setq default-directory dir)
      (setq process (start-file-process buffer-name buffer-name
				   "cscope" "-ld" "-f" "cscope.out"))
      (set-process-filter process 'rscope-filter)
      (set-process-query-on-exit-flag process nil)
      (accept-process-output process 3)
      (if (looking-at ".*cannot open.*cscope\.out.*")
	  (progn
	    (when rscope-buffer	(kill-buffer rscope-buffer))
	    (message "rscope: no cscope.out file here"))
	(progn
	  (rscope-wait-for-output)
	  (message "rscope: database load %s : OK" dir))
	))
    rscope-buffer))

(defun rscope-find-this-symbol (symbol)
  "Locate a symbol in source code."
  (interactive (rscope-interactive
		(list (cons "Find this symbol: " (current-word)))))
  (rscope-handle-query (concat "0" symbol "\n")))

(defun rscope-find-global-definition (symbol)
  "Find a symbol's global definition."
  (interactive (rscope-interactive
		(list (cons "Find this global definition: " (current-word)))))
  (rscope-handle-query (concat "1" symbol "\n")))

(defun rscope-find-called-functions (symbol)
  "Display functions called by a function."
  (interactive (rscope-interactive
		(list (cons "Find functions called by this function: " (current-word)))))
  (rscope-handle-query (concat "2" symbol "\n")))

(defun rscope-find-functions-calling-this-function (symbol)
  "Display functions calling a function."
  (interactive (rscope-interactive
		(list (cons "Find functions calling by this function: " (current-word)))))
  (rscope-handle-query (concat "3" symbol "\n")))

(defun rscope-find-this-text-string (symbol)
  "Locate where a text string occurs."
  (interactive (rscope-interactive
		(list (cons "Find this text string: " (current-word)))))
  (rscope-handle-query (concat "4" symbol "\n")))

(defun rscope-find-files-including-file (symbol)
  "Locate all files #including a file."
  (interactive (rscope-interactive
		(list (cons "Find files #including this file: " (current-word)))))
  (rscope-handle-query (concat "8" symbol "\n")))

(defun rscope-all-symbol-assignments (symbol)
  "Find all the assignments of the symbol"
  (interactive (rscope-interactive
		(list (cons "this don't work due to the bug of cscope, Find all assignments of symbol: " (current-word)))))
  (rscope-handle-query (concat "10" symbol "\n")))

(defun rscope-find-calling-hierarchy (symbol depth)
  "Find all functions calling a function, then functions calling these ones, etc ..."
  (interactive (rscope-interactive
		(list (cons "Find function's calling hierarchy: " (current-word))
		      (cons "Depth: " "4"))))
  (rscope-handle-query-call-hierarchy symbol (string-to-number depth)))

(defun rscope-pop-mark()
  "Pop back to where cscope was last invoked."
  (interactive)
  (if (ring-empty-p rscope-marker-ring)
      (error "There are no marked buffers in the rscope-marker-ring yet"))
  (let* ((marker (ring-remove rscope-marker-ring 0))
	 (old-buffer (current-buffer))
	 (marker-buffer (marker-buffer marker))
	 marker-window
	 (marker-point (marker-position marker))
	 (old-buffer-killable))
    (setq old-buffer-killable
	  (and (with-current-buffer old-buffer
		 (and (boundp 'rscope-auto-open) rscope-auto-open))
	       (not (rscope-ring-bufferp old-buffer))))
    
    (if marker-buffer
	(progn
	  (when old-buffer-killable (kill-buffer old-buffer))
	  (switch-to-buffer marker-buffer)
	  (goto-char marker-point)))
    (set-marker marker nil)
    ))

(defun rscope-interactive (prompt-defvals-alist)
  "Interactive prompter, with an associative list of prompt and default value.
As a side effect, rscope-action-message is set."
  (setq rscope-action-message "")

  (mapcar (lambda (elem)
	    (let ((p (car elem)) (def (cdr elem)) prompt out)
	      (setq prompt
		    (if def (format "%s (default %s): "
				    (substring p 0 (string-match "[ :]+\\'" p)) def)
		      p))
	      (setq out (read-string prompt nil nil (if def def "")))
	      (when (> (length rscope-action-message) 0)
		(setq rscope-action-message
		      (concat rscope-action-message " - ")))
	      (setq rscope-action-message
		    (concat rscope-action-message
			    (format "%s%s" p out)))
	      out))
	  prompt-defvals-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result buffer navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-display-entry-current-window ()
  "Display the entry at point in current window.
Open a new buffer if necessary."
  (interactive)
  (apply 'rscope-display-file-line
	 (append (rscope-get-relative-entry (current-buffer) 0) nil)))

(defun rscope-display-entry-other-window ()
  (interactive)
  "Display the entry at point in other window, without loosing selection."
  (apply 'rscope-display-file-line
	 (append (rscope-get-relative-entry (current-buffer) 0) '(t nil)))
  )

(defun rscope-select-entry-current-window ()
  "Select the entry in the cscope result buffer at current point,
display it in the current window replacing the result buffer."
  (interactive)
  (let ((result-buffer (current-buffer))
	(buffer
	 (apply 'rscope-display-file-line
		(append (rscope-get-relative-entry (current-buffer) 0) '(nil t)))))
    (rscope-clear-previewed-buffers result-buffer buffer)
    ))

(defun rscope-select-entry-other-window ()
  "Select the entry in the cscope result buffer at current point,
display it in the other window, and bury the result buffer."
  (interactive)
  (let* ((result-buffer (current-buffer))
	 (result-window (get-buffer-window result-buffer))
	 (dd default-directory)
	 buffer)
    (and result-window (quit-window nil result-window))
    (let ((default-directory dd))
      (setq buffer (apply 'rscope-display-file-line
			  (append (rscope-get-relative-entry result-buffer 0) '(t t)))))
    (rscope-clear-previewed-buffers result-buffer buffer)))

(defun rscope-preview-entry-other-window ()
  "Preview the entry in another window, without loosing selection, and
with an optionnal arrow to show what was found."
  (interactive)
  (let (buffer file-line file already-opened)
    (setq file-line (rscope-get-relative-entry (current-buffer) 0)
	  file (nth 0 file-line)
	  already-opened (and (get-file-buffer file)
			      (not (member (get-file-buffer file) preview-buffers)))
	  buffer (apply 'rscope-display-file-line (append file-line '(t nil t))))
    (push buffer preview-buffers)
    (when already-opened
      (push buffer preview-already-opened-buffers))
    ))

(defun rscope-next-symbol ()
  "Move to the next symbol in the *rscope* buffer."
  (interactive)
  (rscope-get-relative-entry (current-buffer) +1))

(defun rscope-prev-symbol ()
  "Move to the previous symbol in the *rscope* buffer."
  (interactive)
  (rscope-get-relative-entry (current-buffer) -1))

(defun rscope-close-results ()
  "Close the cscope result buffer, and all previews."
  (interactive)
  (let ((result-buffer (current-buffer)))
    (rscope-clear-previewed-buffers result-buffer)
    (quit-window nil (get-buffer-window result-buffer))))

(defun rscope-regenerate-database ()
  "Regenerate the cscope database."
  (interactive)
  (let* ((result-buffer (current-buffer))
	 (dir (buffer-local-value 'default-directory result-buffer))
	 (procbuf (buffer-local-value 'proc-buffer result-buffer)))
    (rscope-regenerate-cscope-database dir)
    (kill-buffer result-buffer)
    (kill-buffer procbuf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result buffer helpers: internal navigation, buffer spawning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-get-nth-relative-entry (entry-number)
  "Returns the (file . line-number) of the nth relative entry in the result
buffer. This function should only be called inside the result buffer."
  (let (line-number file-name stall
	(direction (if (> entry-number 0) 1 -1)))
					; Be at beginning of line
    (forward-line 0)
    (while (not (= 0 entry-number))
      (setq entry-number (- entry-number direction))
					; Look for next valid entry
      (setq stall 0)
      (while (= 0 stall)
	(setq stall (forward-line direction))
					;(when (looking-at "^\\*\\* ")
					;  (setq stall 1))))
	(setq line-number (get-text-property (point) 'rscope-line-number))
	(when line-number
	  (setq stall 1))))

    (setq line-number (get-text-property (point) 'rscope-line-number))
    (setq file-name (get-text-property (point) 'rscope-file-name))
    (list file-name line-number)))

(defun rscope-get-relative-entry (result-buffer &optional entry-number)
  "Returns the (file . line-number) of the currently selected result.
If entry-number is provided, return the nth next/previous entry in the results."
  (with-current-buffer result-buffer
    (unless entry-number
      (setq entry-number 0))
    (rscope-get-nth-relative-entry entry-number)))

(defun rscope-clear-previewed-buffers (result-buffer &optional spared-buffer)
  "Kills all previewed buffer, sparing only the spared-buffer and the
preview buffers which were already opened before preview.
The spared buffers are cleaned of his arrow."
  (let (all-buffers spared-buffers die-buffers)
    (setq all-buffers
	  (with-current-buffer result-buffer preview-buffers))
    (setq spared-buffers
	  (with-current-buffer result-buffer preview-already-opened-buffers))
    (push spared-buffer spared-buffers)
    (delete-dups all-buffers)
    (delete-dups spared-buffers)

    (setq die-buffers (set-difference all-buffers spared-buffers))
    (dolist (buffer die-buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (dolist (buffer spared-buffers)
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (set-marker overlay-arrow-position nil))))
    (with-current-buffer result-buffer
      (setq preview-buffers '())
      (setq preview-already-opened-buffers '()))
    ))

(defun rscope-get-buffer-file-line (file-name line-number &optional arrowp)
  "Display a (file, line) in either the current window or the other window.
Optionally draw an arrow at the line number."
  (let (already-opened buffer)
    (when (and file-name line-number)
      (setq file-name
	    (expand-file-name file-name default-directory))
      (unless (file-readable-p file-name)
	(error "%s is not readable or exists" file-name))
      (when (get-file-buffer file-name)
	(setq already-opened t))
      (setq buffer (find-file-noselect file-name))
      (with-current-buffer buffer
	(when (not already-opened)
	  (rscope-mark-buffer-opened buffer))
	(goto-char (point-min))
	(forward-line (1- line-number))
	(when (and rscope-allow-arrow-overlays arrowp)
	  (set-marker overlay-arrow-position (point))))
      )
    buffer
    ))

(defun rscope-display-file-line (file-name line-number &optional otherp selectp arrowp)
  "Display a (file, line) in either the current window or the other window.
If selectp, select the buffer. If arrowp, draw an arrow on the line number
on this buffer for the selected entry.
Returns the buffer containing the file."
  (let (window
	(buffer (rscope-get-buffer-file-line file-name line-number arrowp)))
    (if selectp
	(progn
	  (if otherp (pop-to-buffer buffer) (switch-to-buffer buffer))
	  (goto-char (point-min))
	  (forward-line (1- line-number)))
      (progn
	(display-buffer buffer)
	(with-current-buffer buffer
	  (goto-char (point-min))
	  (forward-line (1- line-number)))
	(setq window (get-buffer-window buffer))
	(when window (set-window-point window (with-current-buffer buffer (point))))))
    buffer
    ))

(defun rscope-list-entry-mode ()
  (use-local-map rscope-list-entry-keymap)
  (setq buffer-read-only t
	mode-name "rscope"
	major-mode 'rscope-list-entry-mode
	overlay-arrow-string rscope-overlay-arrow-string
	)
  (or overlay-arrow-position
      (setq overlay-arrow-position (make-marker)))
  (run-hooks 'rscope-list-entry-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Result buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-ring-bufferp (buffer)
  "Check if buffer is on the cscope searches ring."
  (member buffer
	  (mapcar 'marker-buffer (ring-elements rscope-marker-ring))))

(defun rscope-clear-overlay-arrow ()
  "Clean up the overlay arrow."
  (interactive)
  (let ()
    (if overlay-arrow-position
	(set-marker overlay-arrow-position nil))
    ))

(defun rscope-mark-buffer-opened (buffer)
  "Mark a buffer which was opened by a rscope action."
  (with-current-buffer buffer
    (make-local-variable 'rscope-auto-open)
    (setq rscope-auto-open t)
    (add-hook 'first-change-hook (lambda () (setq rscope-auto-open nil)))
    ))

(defun get-strings-prefixed-by (prefix list)
  (delq nil
	(mapcar (lambda (x) (when (string-prefix-p prefix x) x)) list)))

(defun rscope-get-cscope-buffers ()
  (get-strings-prefixed-by "*rscope-"
			   (mapcar (function buffer-name) (buffer-list))))

(defun rscope-find-cscope-process (buffer)
  "Find the initialized (through rscope-init) cscope buffer for buffer.
The match is done by matching the buffer absolute path of the
file with the absolute path of each rscope initialized buffer,
and see if a match appears.

By default, if no match found and if exactly one cscope is launched,
use it."
  (let* ((rscope-buffers (rscope-get-cscope-buffers))
	 exact-match)
    (setq exact-match
	  (car (delq nil (mapcar (lambda(buf)
				   (when (string-prefix-p
					  (expand-file-name (buffer-local-value 'default-directory (get-buffer buf)))
					  (expand-file-name (buffer-local-value 'default-directory (get-buffer buffer))))
				     buf))
				 rscope-buffers))))
    (cond
     (exact-match exact-match)
     ((setq exact-match (rscope-find-cscope-process-run-hooks buffer)) exact-match)
     ((= 1 (length rscope-buffers)) (car rscope-buffers))
     ((error "No rscope initialized found, did you call rscope-init ?")))))

(defun rscope-find-cscope-process-run-hooks (buffer)
  "Hooks to find a directory where a cscope.out file might be.
The first hook returning non nil wins."
  (let* ((dir (run-hook-with-args-until-success 'rscope-autoinit-cscope-dir-hooks
						buffer))
	 (rscope-buffer (when dir (rscope-init dir))))
    rscope-buffer))

(defun rscope-select-unique-result ()
  "Called when query returned only 1 result, and display window"
  (let (l file-name line-number)
    (with-current-buffer (get-buffer-create rscope-output-buffer-name)
      (goto-char (point-min))
      (setq l (rscope-get-relative-entry (current-buffer) +1))
      (setq file-name (nth 0 l))
      (setq line-number (nth 1 l))
      (if (and file-name line-number)
	  (rscope-select-entry-other-window)
	(error "No cscope unique entry found, that's abnormal")))))

(defun rscope-handle-query (query)
  "Launch the query in the rscope process."
  (let (nb-results result-buf
		   (rscope-process (rscope-find-cscope-process (current-buffer))))
    (setq result-buf
	  (rscope-create-result-buffer rscope-action-message rscope-process))
    (when rscope-process
	(progn
	  (setq nb-results
		(rscope-cscope-exec-query rscope-process query))
	  (rscope-cscope-parse-output rscope-process
				      result-buf 'rscope-results-organize-filename)
	  (when (>= nb-results 1)
	    (ring-insert rscope-marker-ring (point-marker)))
	  (rscope-finish-result-buffer result-buf)
	  (when (= 1 nb-results) (rscope-select-unique-result))))))

(defun rscope-handle-query-call-hierarchy (function-name levels)
  "Launch the query to get a calling hierarchy in rscope process."
  (let (result-buf regexp found nb-lines
		   (rscope-process (rscope-find-cscope-process (current-buffer))))
    (when rscope-process
	(progn
	  (setq result-buf (rscope-create-result-buffer
			    rscope-action-message rscope-process))
	  (with-current-buffer result-buf
	    (rscope-results-insert-function function-name 1 0 "" "File")
	    (make-local-variable 'rscope-level)

	    (dotimes (level levels)
	      (setq regexp (format "^[*]\\{%d\\}" (+ 1 level))
		    rscope-level (+ 2 level))
	      (goto-char (point-min))
	      (setq found (re-search-forward regexp nil t))
	      (while found
		(forward-line 0)
		(setq function-name (get-text-property (point) 'rscope-function-name))
		(forward-line +1)
		(setq nb-lines
		      (rscope-cscope-exec-query rscope-process
						(concat "3" function-name "\n")))
		(rscope-cscope-parse-output rscope-process
					    result-buf 'rscope-results-organize-funcs)
		(setq found (re-search-forward regexp nil t)))
	      ))
	  (rscope-finish-result-buffer result-buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cscope automatic finders/generators of cscope database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-regenerate-cscope-database (dir &optional args)
  "Regenerate the cscope.out database from a directory root.
Only consider *.c and *.h files."
  (message "Rscope: generating cscope database in : %s" dir)
  (let* ((default-directory (if (string-suffix-p "/" dir) dir (concat dir "/")))
	 (exit-code
	  (process-file-shell-command
	   (format "find -name '*.[ch]' -o -name '*.cpp' > cscope.files && cscope -b -q %s"
		   (concat args)))))
    (if (and (numberp exit-code) (= 0 exit-code))
	(concat dir "/")
      (error "Cscope database generation failed, exit code=%d." exit-code))))

(defun rscope-autoinit-path-upwards-cscope_out (buffer)
  "Look the directory tree upwards, and report the first directory containing
a file named cscope.out."
  (let (found old-dir (dir (buffer-local-value 'default-directory buffer)))
    (while (and dir (not found) (not (string= old-dir dir)))
      (setq old-dir dir)
      (if (file-readable-p (concat dir "cscope.out"))
	  (setq found dir)
	(setq dir (file-name-directory (directory-file-name dir)))))
    found))

;; Advanced cscope.out generator looking for autotools configure.ac
(defun rscope-autoinit-path-upwards-configure_ac (buffer)
  "Look the directory tree upwards, and report the first directory containing
a file named configure.ac."
  (let (found old-dir (dir (buffer-local-value 'default-directory buffer)))
    (while (and dir (not found) (not (string= old-dir dir)))
      (setq old-dir dir)
      (if (file-readable-p (concat dir "configure.ac"))
	  (setq found (rscope-regenerate-cscope-database dir))
	(setq dir (file-name-directory (directory-file-name dir)))))
    found))

;; Advanced cscope.out generator looking for git toplevel tree
(defun rscope-autoinit-git-toplevel (buffer)
  "If in a git tree, generate the cscope database at git toplevel."
  (let ((toplevel (with-current-buffer buffer
		    (replace-regexp-in-string "\n$" ""
					      (shell-command-to-string "git rev-parse --show-toplevel"))))
	(tramp-vec))
    (when (and (featurep 'tramp) (tramp-tramp-file-p default-directory))
      (setq tramp-vec (tramp-dissect-file-name default-directory))
      (setq toplevel (tramp-make-tramp-file-name
		      (tramp-file-name-method tramp-vec)
		      (tramp-file-name-user tramp-vec)
		      (tramp-file-name-host tramp-vec)
		      toplevel
		      (tramp-file-name-hop tramp-vec))))
    (when (file-exists-p (directory-file-name toplevel))
      (rscope-regenerate-cscope-database toplevel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cscope process running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-filter (process string)
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(defun rscope-wait-for-output (&optional timeout)
  (let ((proc (get-buffer-process (current-buffer)))
	(found nil)
	(start-time (current-time))
	(start-point (point))
	(nb-lines 0))
    (save-excursion
      (while (not found)
	;(debug)
	(accept-process-output proc 1)
	;(debug)
	(goto-char (point-max)) ;move the last line
	(forward-line 0) ;move to the beggining of last line
	(setq found (looking-at "^>>")) ;looking for cscope prompt "^>>"
	(when (not (process-live-p proc))
	  (error "Cscope process died, kill %s buffer please." (current-buffer)))))

    ;; Find the number of results returned by the search
    (goto-char start-point)
    (when (re-search-forward "^cscope: \\([0-9]+\\) lines$" nil t)
      (setq nb-lines (string-to-number (match-string 1)))
      (forward-line +1))
    nb-lines))

(defun rscope-cscope-exec-query (procbuf command)
  (let ((nb-lines 0)
	(proc (get-buffer-process procbuf)))
    (with-current-buffer procbuf
      (goto-char (point-max))
      (insert "\n")
      (insert command)
      (process-send-string proc command)
      (setq nb-lines (rscope-wait-for-output))
      nb-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rscope cscope output parsing and result buffer creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope-cscope-parse-output (procbuf resultbuf organizer)
  "Process a cscope raw output in procbuf, between point [(point)..(point-max)].
Parse each line, and once file, line number and line content are parsed,
call organizer to handle them within resultbuf."
  (let (line file line-number function-name content
	     (stall 0)
	     (cscope-regexp
	      "\\([^[:blank:]]*\\)[[:blank:]]+\\([^[:blank:]]*\\)[[:blank:]]+\\([[:digit:]]*\\)[[:blank:]]+\\(.*\\)"
	      ))
    (with-current-buffer procbuf
      (while (and (< (point) (point-max)) (= 0 stall))
	(setq line (buffer-substring-no-properties (line-beginning-position)
						   (line-end-position)))
	(setq stall (forward-line 1))

					; Match a cscope output line
	(when (string-match cscope-regexp line)
	  (setq file (match-string 1 line))
	  (setq function-name (match-string 2 line))
	  (setq line-number (string-to-number (match-string 3 line)))
	  (setq content (match-string 4 line))
	  (apply organizer (list resultbuf file line-number function-name content))
	  )))))

(defun rscope-results-insert-filename (file-name level)
  (insert (propertize "*"
		      'rscope-file-name file-name))
  (insert-char ?* (- level 1))
  (insert " ")
  (insert (propertize file-name
		      'face 'rscope-file-face
		      'rscope 'rscope-file-name))
  (insert "\n")
  )

(defun rscope-results-insert-function (function-name level line-number content
						     file-name &optional write-file-p)
  (let (str displayed-file-name)
    (setq displayed-file-name
	  (if rscope-hierarchies-shorten-filename
	      (file-name-nondirectory file-name)
	      file-name))
    (insert (propertize "*"
			'rscope-function-name function-name
			'rscope-line-number line-number
			'rscope-file-name file-name))
    (insert-char ?* (- level 1))
    (insert " ")
    (setq str (concat
	       (propertize function-name 'face 'rscope-function-face)
	       "() ["
	       (when write-file-p
		 (concat 
		  (propertize displayed-file-name 'face 'rscope-file-face)
		  ":"))
	       (propertize (number-to-string line-number) 'face 'rscope-line-number-face)
	       "]"
	       (propertize content 'face 'rscope-line-face)
	       "\n"))
    (insert str)))

(defun rscope-results-organize-filename (buf file line-number function-name content)
  "Insert in buffer buf the entry, where all functions are grouped by file."
  (let ((found)
	(tramp-file
	 (when (tramp-tramp-file-p default-directory)
	   (with-parsed-tramp-file-name default-directory tp
	     (tramp-make-tramp-file-name tp-method tp-user tp-host
					 (if (file-name-absolute-p file)
					     file
					   (concat tp-localname file)))))))
    (with-current-buffer (get-buffer-create buf)
      ;; Find the file in buf if already present
      (goto-char (point-min))
      (setq found (re-search-forward (format "^\* %s" file) nil t))
      (forward-line +0)
      
      ;;; If found the file, move to the next line, beggining of line
      ;;; Else insert the new filename
      (unless found
	(goto-char (point-max))
	(rscope-results-insert-filename (or tramp-file file) 1)
	(forward-line -1))
      (forward-line +1)

      ;;; Insert the found result
      (rscope-results-insert-function function-name 2 line-number content
				      (or tramp-file file))
      )))

(defun rscope-results-organize-funcs (buf file line-number function-name content)
  "Insert in buffer buf the entry, where all functions are not grouped."
  (let ((found)
	(tramp-file
	 (when (tramp-tramp-file-p default-directory)
	   (with-parsed-tramp-file-name default-directory tp
	     (tramp-make-tramp-file-name tp-method tp-user tp-host
					 (if (file-name-absolute-p file)
					     file
					   (concat tp-localname file)))))))
    (with-current-buffer buf
      (rscope-results-insert-function function-name rscope-level line-number content
				      (or tramp-file file) t))))

(defun rscope-create-result-buffer (header procbuf)
  (when (get-buffer rscope-output-buffer-name)
    (kill-buffer rscope-output-buffer-name))
  (let ((result-buf (get-buffer-create rscope-output-buffer-name)))
    (with-current-buffer result-buf
      (when (featurep 'outline)
	    (outline-minor-mode))
      (make-local-variable 'preview-buffers)
      (make-local-variable 'preview-already-opened-buffers)
      (make-local-variable 'proc-buffer)
      (setq preview-buffers '()
	    preview-already-opened-buffers '())
      (setq default-directory
	    (buffer-local-value 'default-directory (get-buffer  procbuf)))
      (setq proc-buffer procbuf)
      (when header
	(insert header "\n"))
      (insert rscope-separator-line "\n"))
    result-buf))

(defun rscope-finish-result-buffer (result-buf)
  (with-current-buffer result-buf
    (goto-char (point-max))
    (insert rscope-separator-line "\n")
    (insert "Search complete.")
    (pop-to-buffer result-buf)
    (goto-char (point-min))
    (rscope-get-relative-entry (current-buffer) +1)
    (rscope-list-entry-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rscope minor mode hook: provides rscope:keymap for key shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rscope:hook ()
  (global-set-key (kbd "\C-cs") 'rscope:map))

(add-hook 'c-mode-hook (function rscope:hook))
(add-hook 'c++-mode-hook (function rscope:hook))
(add-hook 'dired-mode-hook (function rscope:hook))

;; By default, ease users life by auto-loading cscope database by some simple
;; methods :
;;   - directory tree upwards traversal looking for cscope.out file

(add-hook 'rscope-autoinit-cscope-dir-hooks
	  (function rscope-autoinit-git-toplevel))

(add-hook 'rscope-autoinit-cscope-dir-hooks
	  (function rscope-autoinit-path-upwards-configure_ac))

(add-hook 'rscope-autoinit-cscope-dir-hooks
	  (function rscope-autoinit-path-upwards-cscope_out))

(provide 'rscope)
;;; rscope.el ends here


;;; rscope-nav.el --- Markers navigation tool for rscope
;;
;; Copyright(c) 2015 Sylvain Chouleur <sylvain.chouleur@gmail.com>
;; Authors: Sylvain Chouleur <sylvain.chouleur@gmail.com>
;; Keywords: code
;; Licence: GPLv2
;; Version 0.1
;;
;; rscope-nav-mode is a major mode to help viewing and manipulating
;; rscope marker ring

;; Usage:
;;   Do some rscope tag lookups without popping them
;;   M-x rscope-nav
;;     This command will open the rscope navigation buffer, displaying
;;     the ring of markers from the newest to the oldest.

;; Navigation buffer (*rscope-nav*) bindings:
;;   "n": Next entry
;;   "p": Previous entry
;;   "q": Quit window
;;   "g": Refresh
;;   "k": Kill current entry
;;   "K": Flush marker ring
;;   "s": Set current entry as the newest one. All newer entries are
;;   deleted
;;   "RET": Goto current entry

;; this file is *NOT* part of GNU Emacs.
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

(defvar rscope-nav-buffer-name "*rscope-nav*"
  "The name of the rscope navigation buffer.")

(defvar rscope-nav-keymap
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "n" 'forward-line)
    (define-key map "p" 'rscope-nav-previous-entry)
    (define-key map "q" 'rscope-nav-quit)
    (define-key map "g" 'rscope-nav-refresh)
    (define-key map "k" 'rscope-nav-kill)
    (define-key map "K" 'rscope-nav-flush)
    (define-key map "s" 'rscope-nav-set-head)
    (define-key map " " 'rscope-nav-preview-entry)
    (define-key map (kbd "RET") 'rscope-nav-open-entry)
    (define-key map (kbd "S-<return>") 'rscope-nav-open-entry-current-window)
    map))

(defvar rscope-nav-previewed nil
  "Flag indicating if rscope-nav has set the preview marker")

(defun rscope-nav-previous-entry ()
  "Move point to the previous entry"
  (interactive)
  (forward-line -1))

(defun rscope-nav-mode ()
  "Major mode to navigate in rscope markers"
  (interactive)
  (use-local-map rscope-nav-keymap)
  (setq buffer-read-only t
	mode-name "rscope-nav"
	major-mode 'rscope-nav-mode))

(defun rscope-nav ()
  "List the rscope markers stack and navigate into it"
  (interactive)
  (pop-to-buffer (get-buffer-create rscope-nav-buffer-name))
  (rscope-nav-refresh))

(defun rscope-nav-quit ()
  "Quit *rscope-nav* window"
  (interactive)
  (when rscope-nav-previewed
    (set-marker overlay-arrow-position nil))
  (quit-window))

(defun rscope-nav-get-current-entry()
  "Return marker associated with the entry where point is"
  (get-text-property (line-beginning-position) 'rscope-nav-marker))

(defun rscope-nav-open-entry (&optional current-window preview)
  "Open current entry of rscope navigation buffer"
  (interactive)
  (let ((mark (rscope-nav-get-current-entry)))
    (unless mark
      (error "Invalid entry"))
    (let ((buf (marker-buffer mark))
	  win)
      (if current-window
	  (and (switch-to-buffer buf nil t)
	       (setq win (selected-window)))
	(setq win (display-buffer buf t)))
      (with-selected-window win
	(goto-char mark)
	(when (and preview rscope-allow-arrow-overlays)
	  (progn (set-marker overlay-arrow-position (line-beginning-position) buf)
		 (setq rscope-nav-previewed t))))
      (unless preview
	(switch-to-buffer buf)
	(when rscope-nav-previewed
	  (set-marker overlay-arrow-position nil)
	  (setq rscope-nav-previewed t))))))

(defun rscope-nav-open-entry-current-window ()
  "Open current entry of rscope navigation buffer in current window"
  (interactive)
  (rscope-nav-open-entry t))

(defun rscope-nav-preview-entry ()
  "Open current entry of rscope navigation buffer without focusing on the buffer"
  (interactive)
  (rscope-nav-open-entry nil t))

(defun rscope-nav-set-head ()
  "Set current entry as head of the ring"
  (interactive)
  (let ((mark (rscope-nav-get-current-entry)))
    (unless mark
      (error "Invalid entry"))
    (let ((index (ring-member rscope-marker-ring mark)))
      (unless index
	(error "Entry not part of rscope-marker-ring"))
      (dotimes (count index)
	(ring-remove rscope-marker-ring 0))
      (rscope-nav-refresh))))

(defun rscope-nav-kill ()
  "Remove current entry from the ring"
  (interactive)
  (let ((mark (rscope-nav-get-current-entry)))
    (ring-remove rscope-marker-ring (ring-member rscope-marker-ring mark)))
  (rscope-nav-refresh))

(defun rscope-nav-flush ()
  "Flush rscope-marker-ring"
  (interactive)
  (dotimes (count (ring-length rscope-marker-ring))
    (ring-remove rscope-marker-ring 0))
  (rscope-nav-refresh))

(defun rscope-nav-refresh ()
  "Refresh *rscope-nav* buffer"
  (interactive)
  (with-current-buffer (get-buffer-create rscope-nav-buffer-name)
    (save-excursion
      (read-only-mode -1)
      (erase-buffer)
      (dolist (mark (delq nil (ring-elements rscope-marker-ring)))
	(when mark
	  (let ((buf (marker-buffer mark)))
	    (if buf
		(insert
		 (propertize (or (buffer-file-name buf) (buffer-name buf))
			     'face 'rscope-file-face
			     'rscope-nav-marker mark)
		 ":"
		 (propertize (number-to-string (with-current-buffer buf (line-number-at-pos (marker-position mark))))
			     'face 'rscope-line-number-face)
		 " "
		 (with-current-buffer buf (save-excursion (goto-char mark) (buffer-substring (line-beginning-position) (line-end-position))))
		 "\n")
	      (insert (propertize "<No buffer>\n" 'face 'error))))))
      (rscope-nav-mode))))

(provide 'rscope-nav)

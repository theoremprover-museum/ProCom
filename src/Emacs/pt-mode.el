;;; pt-mode.el --- A major mode for editing ProTop scrips and inferior ProTop
;;;****************************************************************************
;;; $Id: pt-mode.el,v 1.2 1995/05/04 13:07:55 gerd Exp $
;;;****************************************************************************
;;;
;;; Description:
;;;
;;;	This file implements a major mode for editing ProTop script files.
;;;	ProTop is run as an inferior process with an interaction
;;;	similar to the shell mode.
;;;
;;; Installation:
;;;	Put the following lines in your .emacs file:
;;;
;;;	    (require 'pt-mode)
;;;         (setq protop-program "PROTOP-FULL-PATH")
;;;
;;;	where PROTOP-FULL-PATH is the name of the ProTop executable
;;;	including the full path.
;;;
;;;	This assumes that the file pt-mode.el can be found on
;;;	the load-path:
;;;
;;;	    (require 'cl)
;;;	    (setq load-path (adjoin "PT-MODE-DIRECTORY" load-path))
;;;
;;;	Then you can activate the automatic recognition of PropTop script
;;;	files by their extension (.pt):
;;;
;;;	    (setq auto-mode-alist (adjoin (cons "\\.pt$" pt-mode)
;;;					  auto-mode-alist))
;;;
;;;
;;; Bugs and Problems:
;;; To do:
;;; Changes:
;;;
;;; Author:	
;;;	Gerd Neugebauer
;;;	Ödenburger Str. 16
;;;	64295 Darmstadt (Germany)
;;;	Net: gerd@imn.th-leipzig.de
;;;
;;;****************************************************************************
;;; LCD Archive Entry:
;;; Not yet.
;;;****************************************************************************
;;;
;;; Copyright (C) 1995 Gerd Neugebauer
;;;
;;; pt-mode.el is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU General Public
;;; License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; pt-mode.el, but only under the conditions described in the
;;; GNU General Public License.	 A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.	Among other things, the copyright notice
;;; and this notice must be preserved on all copies.
;;;

;;;----------------------------------------------------------------------------

(require 'comint)
(eval-when-compile '(require 'comint))

(defvar protop-program "protop" "Name of the program to start ProTop.")

(defvar protop-command-list '("protop"))

(defvar protop-buffer "*protop*")

(defvar protop-startfile nil)

(defvar protop-use-frames nil
  "Boolean to indicate if a frame should be used for a ProTop process.")

(defvar protop-include-command "include('%s').\n" 
  "Format of the command send to the inferior ProTop when a file should be
included. The %s contained will be expanded to the full file name.

NOTE: This format string has to end with .\\n")

(defvar protop-use-net nil
  "Boolean to indicate if the user should have the possibility to start a
ProTop process on other hosts.

It is assumed that the other host shares the directory structure with the
current host and that rsh or remsh can be used without typing a password.") 

(defvar pt-mode-map nil "Keymap used in pt-mode.")

(if pt-mode-map
    nil
  (setq pt-mode-map (make-sparse-keymap))
  (define-key pt-mode-map "\C-c\C-r" 'run-protop    )
  (define-key pt-mode-map "\C-c\C-c" 'save-and-send-to-protop    )
  (define-key pt-mode-map [menu-bar pt-] 
    (cons "ProTop" (make-sparse-keymap "ProTop")))
  (define-key pt-mode-map [menu-bar pt- -pt-kill]
    '("Kill ProTop process" . kill-protop))
  (define-key pt-mode-map [menu-bar pt- -pt-restart]
    '("Restart ProTop process" . restart-protop))
  (define-key pt-mode-map [menu-bar pt- -pt-show]
    '("Show ProTop window" . protop-show-window))
  (define-key pt-mode-map [menu-bar pt- -pt-frame]
    '("Show ProTop frame" . protop-show-frame))
)

(defun pt-mode ()
  "Major mode for editing .pt files.

Special commands:
\\{pt-mode-map}

Mode variables:

protop-use-frames	Boolean to indicate if a frame should be used for a
 			ProTop process.

protop-program		Name of the program to start ProTop.

Entering PT mode calls the value of prolog-mode-hook and then the value
of pt-mode-hook."
  (interactive)
  (prolog-mode)
  (use-local-map pt-mode-map)
  (setq mode-name "PT")
  (setq major-mode 'pt-mode)
  (make-local-variable 'protop-buffer)
  (setq protop-buffer "*protop*")
  (make-local-variable 'protop-command-list)
  (setq protop-command-list (list protop-program))
  (run-hooks 'pt-mode-hook)
)

(defun protop-show-frame ()
  "Enable the use of frames to display the inferior ProTop process.
The frame is displayed if no window is visible already."
  (interactive)
  (setq protop-use-frames t)
  (protop-show-window-or-frame)
)

(defun protop-show-window ()
  "Enable the use of window display the inferior ProTop process.
The window is displayed if no frame is visible already."
  (interactive)
  (setq protop-use-frames nil)
  (protop-show-window-or-frame)
)

(defun protop-show-window-or-frame ()
  "A inferior ProTop buffer is shown.
The variable protop-use-frames determines wether to use a window or a frame."
  (interactive)
  (if protop-buffer
      (if protop-use-frames
	  (switch-to-buffer-other-frame  protop-buffer)
	(switch-to-buffer-other-window protop-buffer)))
)

(defvar protop-last-host-name "localhost")

(defun run-protop ()
  "Switch to a running ProTop process or create one if none exists."
  (interactive)
  (let ((name    "protop")
	(command (list protop-program)))
    (if (and protop-buffer (get-buffer protop-buffer))
	nil
      (if protop-use-net
	  (let ((host (format "%s"
			      (read-minibuffer "ProTop host: "
					       protop-last-host-name))))
	    (setq protop-last-host-name (format "%s" host))
	    (if (or (string-equal protop-last-host-name "localhost")
		    (string-equal protop-last-host-name ""))
		(setq name "protop"
		      protop-command-list (list protop-program 
						protop-startfile))
	      (setq name (format "protop@%s" host)
		    protop-command-list (list "rsh" 
					      protop-startfile 
					      host 
					      protop-program)))
	    (setq command       protop-command-list
		  protop-buffer (format "*%s*" name)))
	(setq protop-buffer (format "*%s*" name))))
    
    (protop-show-window-or-frame)
    (eval (cons 'make-comint (cons name command)))
    (inferior-protop-mode)
    )
)

(defun kill-protop ()
  "Delete the inferior ProTop buffer if one exists."
  (interactive)
  (if (and protop-buffer (get-buffer protop-buffer))
      (kill-buffer protop-buffer))
  (setq protop-buffer nil)
)

(defun restart-protop ()
  "Kill the inferior ProTop process and start a new one.
Thus it is possible to access the history within the new process."
  (interactive)
  (if (and protop-buffer (get-buffer protop-buffer))
      (kill-process protop-buffer))
  (run-protop)
)

(defun save-and-send-to-protop ()
  "Save the current buffer if required. Afterwards a window or frame with a
ProTop process is opened and the initial buffer is send to this process."
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name))
	(running (and protop-buffer
		      (get-buffer protop-buffer)
		      (equal 'run (process-status protop-buffer)))))
    (if (not running)
	(run-protop)
      (protop-show-window-or-frame))
    (end-of-buffer)
    (if file
	(comint-send-string
	 protop-buffer
	 (format protop-include-command file))
      )
    )
)

;defvar inferior-protop-mode-map nil "")
;
;(if inferior-protop-mode-map
;    nil
;  (setq inferior-protop-mode-map (copy-keymap comint-mode-map))
;  (define-key inferior-protop-mode-map "\C-c\C-r" 'protop-run    )
;)

(defun inferior-protop-mode ()
  "Major mode for interacting with an inferior ProTop process."
  (interactive)
  (comint-mode)
;  (use-local-map inferior-protop-mode-map)
  (setq mode-name  "ProTop")
  (setq major-mode 'inferior-protop-mode)
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "^ProTop -> ")
)

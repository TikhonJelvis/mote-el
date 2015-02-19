(require 'json)

(defvar slick/program "slick")
(defvar slick/process nil)

(defun slick/init ()
  "Starts the slick process if it isn't already running."
  (unless (and slick/process (eq (process-status slick/process) 'run))
    (setq slick/process (start-process "slick" "*slick IO*" slick/program))))

(defun slick/send (message)
  "Send the given string to the active slick process."
  ;; Make sure the process is up and running:
  (slick/init)
  (process-send-string slick/process (concat (json-encode `("Load" ,(buffer-file-name))) "\n"))
  (process-send-string slick/process (concat (json-encode message) "\n")))

(defun slick/command (command &rest args)
  "Send a command as a list containing the name and arguments."
  (message (json-encode `(,command . ,args)))
  (slick/send `(,command . ,args)))

(defun slick/client-state ()
  "Returns the current file and cursor position."
  `(:path ,(buffer-file-name)
    :cursorPos (,(line-number-at-pos) ,(+ (current-column) 1))))

(defun slick/load ()
  (interactive)
  "Loads the current file into the running slick process."
  (slick/command "Load" (buffer-file-name)))

(defun slick/enter-hole ()
  "Enters the hole at the current cursor position."
  (interactive)
  (slick/command "EnterHole" (slick/client-state)))

(defun slick/next-hole ()
  "Jumps to the next hole position, if any."
  (interactive)
  (slick/command "NextHole" (slick/client-state)))

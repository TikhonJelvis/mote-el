(require 'json)

(defvar slick/program "slick")
(defvar slick/process nil)

(defvar slick/input '())
(defvar slick/output '())

(defun slick/insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    ;; Add to output list and execute if it parses:
    (let* ((json-array-type 'list)
           (output (json-read-from-string string)))
      (unless (equal output '("Ok"))
        (add-to-list 'slick/output output)
        (slick/execute output)))

    ;; Write to the process buffer:
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun slick/init ()
  "Starts the slick process if it isn't already running."
  (unless (and slick/process (eq (process-status slick/process) 'run))
    (setq slick/process (start-process "slick" "*slick IO*" slick/program))
    (set-process-filter slick/process 'slick/insertion-filter)))

(defun slick/stop ()
  "Kills the slick process."
  ;; TODO: Kill the process more gracefully?
  (when slick/process (delete-process slick/process)))

(defun slick/restart ()
  "Kills and then inits the slick process."
  (slick/stop)
  (slick/init))

(defun slick/execute (command)
  "Executes a command passed back from Slick. If the given
command isn't recognized, doesn't do anything."
  (message "executing")
  (pcase command
    (`("SetCursor" (,x ,y))
     (message "Moving!")
     (goto-line x)
     (forward-char (- y 1)))))

(defun slick/send (message)
  "Send the given string to the active slick process. Before
sending the actual command, ensure the process is running and
loads the current file."
  ;; Make sure the process is up and running:
  (slick/init)
  (process-send-string slick/process (concat (json-encode `("Load" ,(buffer-file-name))) "\n"))
  (process-send-string slick/process (concat (json-encode message) "\n")))

(defun slick/command (command &rest args)
  "Send a command as a list containing the name and arguments."
  (add-to-list 'slick/input (json-encode `(,command . ,args)))
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

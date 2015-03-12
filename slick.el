(require 'json)

(defvar slick/program "slick")
(defvar slick/process nil)

(defvar slick/input '())
(defvar slick/output '())

;; TODO: This probably exists as a standard function somewhere...
(defun slick/log (list element)
  (set list (cons element (symbol-value list))))

(defun slick/insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    ;; Process output line by line:
    (dolist (line (split-string string "\n"))
      (unless (equal line "")
        (let* ((json-array-type 'list)
               (output (json-read-from-string line)))
          (unless (equal output '("Ok"))
            (slick/log 'slick/output output)
            (slick/execute output)))))

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
  (setq slick/output nil)
  (setq slick/input nil)
  (slick/init))

(defun slick/execute (command)
  "Executes a command passed back from Slick. If the given
command isn't recognized, doesn't do anything."
  (pcase command
    (`("SetCursor" (,x ,y))
     (goto-line x)
     (forward-char (- y 1)))
    (`("SetInfoWindow" ,text)
     (message text))
    (`("Error" "nohole")
     (message "No current hole."))
    (x (message (format "%s" x)))))

(defun slick/load ()
  (interactive)
  "Loads the current file into the running slick process."
  (process-send-string slick/process (concat (json-encode `("Load" ,(buffer-file-name))) "\n")))

(defun slick/send (command)
  "Send the given string to the active slick process. Before
sending the actual command, ensure the process is running and
loads the current file."
  ;; Make sure the process is up and running:
  (slick/init)
  (process-send-string slick/process (concat (json-encode command) "\n")))

(defun slick/command (command &rest args)
  "Send a command as a list containing the name and
arguments. Sleeps for 50ms after sending to let the output get
processed."
  (slick/log 'slick/input (json-encode `(,command . ,args)))
  (slick/send `(,command . ,args))
  (sleep-for 0 50))

(defun slick/client-state ()
  "Returns the current file and cursor position."
  `(:path ,(buffer-file-name)
    :cursorPos (,(line-number-at-pos) ,(+ (current-column) 1))))

(defun slick/enter-hole ()
  "Enters the hole at the current cursor position."
  (interactive)
  (slick/command "EnterHole" (slick/client-state)))

(defun slick/next-hole ()
  "Jumps to and enters the next hole position and enters that
hole, if any."
  (interactive)
  (slick/load)
  (slick/command "NextHole" (slick/client-state))
  (slick/command "EnterHole" (slick/client-state)))

(defun slick/prev-hole ()
  "Jumps to and enters the previous hole position and enters that
hole, if any."
  (interactive)
  (slick/load)
  (slick/command "PrevHole" (slick/client-state))
  (slick/command "EnterHole" (slick/client-state)))

(defun slick/get-env ()
  "Gets the type of the currently entered hole and any relevant
bindings in its scope."
  (interactive)
  (slick/command "GetEnv" (slick/client-state)))

(defun slick/case-further (identifier)
  (slick/command "CaseFurther" identifier (slick/client-state)))

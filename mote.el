(require 'json)

(defvar mote/program "mote")
(defvar mote/process nil)

(defvar mote/input '())
(defvar mote/output '())

(defvar mote/callbacks '())

(defvar mote/default-options '(:sendOutputAsData :json-false :withSuggestions :json-false))

;; TODO: This probably exists as a standard function somewhere...
(defun mote/log (list element)
  (set list (cons element (symbol-value list))))

(defun mote/insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    ;; Process output line by line:
    (dolist (line (split-string string "\n" t))
      (unless (equal line "")
        (let* ((json-array-type 'list)
               (output (json-read-from-string line)))
          (unless (equal output '("Ok"))
            (mote/log 'mote/output output)
            (mote/execute output))))
      (when mote/callbacks
        (let ((fun (last mote/callbacks)))
          (funcall (car fun))
          (setq mote/callbacks (butlast mote/callbacks)))))

    ;; Write to the process buffer:
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun mote/init ()
  "Starts the mote process if it isn't already running."
  (unless (and mote/process (eq (process-status mote/process) 'run))
    (setq mote/process (start-process "mote" "*mote IO*" mote/program))
    (set-process-filter mote/process 'mote/insertion-filter)))

(defun mote/stop ()
  "Kills the mote process."
  ;; TODO: Kill the process more gracefully?
  (when mote/process
    (delete-process mote/process)
    (setq mote/callbacks nil)))

(defun mote/restart ()
  "Kills and then inits the mote process."
  (mote/stop)
  (setq mote/output nil)
  (setq mote/input nil)
  (mote/init))

(defun mote/execute (command)
  "Executes a command passed back from Mote. If the given
command isn't recognized, doesn't do anything."
  (pcase command
    (`("SetCursor" (,x ,y))
     (message "Setting cursor!")
     (goto-line x)
     (forward-char (- y 1)))
    (`("SetInfoWindow" ,text)
     (message text))
    (`("Error" "nohole")
     (message "No current hole."))
    (x (message (format "%s" x)))))

(defun mote/load ()
  (interactive)
  "Loads the current file into the running mote process."
  (push '(lambda () nil) mote/callbacks)
  (process-send-string mote/process (concat (json-encode `("Load" ,(buffer-file-name))) "\n")))

(defun mote/send (command &optional callback)
  "Send the given string to the active mote process. Before
sending the actual command, ensure the process is running and
loads the current file."
  ;; Make sure the process is up and running:
  (mote/init)
  (process-send-string mote/process (concat (json-encode command) "\n"))
  (push (or callback '(lambda () nil)) mote/callbacks))

(defun mote/command (command args &optional callback)
  "Send a command as a list containing the name and
arguments. Sleeps for 50ms after sending to let the output get
processed."
  (mote/log 'mote/input (json-encode `(,command . ,args)))
  (mote/send `(,command . ,args) callback))

(defun mote/client-state ()
  "Returns the current file and cursor position."
  `(:path ,(buffer-file-name)
    :cursorPos (,(line-number-at-pos) ,(+ (current-column) 1))))

(defun mote/enter-hole ()
  "Enters the hole at the current cursor position."
  (interactive)
  (mote/command "EnterHole" (list (mote/client-state))))

(defun mote/next-hole ()
  "Jumps to and enters the next hole position and enters that
hole, if any."
  (interactive)
  (mote/load)
  (mote/command "NextHole" (list (mote/client-state)) (indirect-function 'mote/enter-hole)))

(defun mote/prev-hole ()
  "Jumps to and enters the previous hole position and enters that
hole, if any."
  (interactive)
  (mote/load)
  (mote/command "PrevHole" (list (mote/client-state)))
  (mote/enter-hole))

(defun mote/hole-info ()
  "Gets the type of the currently entered hole and any relevant
bindings in its scope."
  (interactive)
  (mote/command "GetHoleInfo" (list (mote/client-state) mote/default-options)))

(defun mote/case-further (identifier)
  (mote/command "CaseFurther" identifier (list (mote/client-state))))

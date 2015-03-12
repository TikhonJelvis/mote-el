(require 'json)

(defvar slick/program "slick")
(defvar slick/process nil)

(defvar slick/input '())
(defvar slick/output '())

(defvar slick/callbacks '())

;; TODO: This probably exists as a standard function somewhere...
(defun slick/log (list element)
  (set list (cons element (symbol-value list))))

(defun slick/insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    ;; Process output line by line:
    (dolist (line (split-string string "\n" t))
      (unless (equal line "")
        (let* ((json-array-type 'list)
               (output (json-read-from-string line)))
          (unless (equal output '("Ok"))
            (slick/log 'slick/output output)
            (slick/execute output))))
      (when slick/callbacks
        (let ((fun (last slick/callbacks)))
          (funcall (car fun))
          (setq slick/callbacks (butlast slick/callbacks)))))

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
  (when slick/process
    (delete-process slick/process)
    (setq slick/callbacks nil)))

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
     (message "Setting cursor!")
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
  (push '(lambda () nil) slick/callbacks)
  (process-send-string slick/process (concat (json-encode `("Load" ,(buffer-file-name))) "\n")))

(defun slick/send (command &optional callback)
  "Send the given string to the active slick process. Before
sending the actual command, ensure the process is running and
loads the current file."
  ;; Make sure the process is up and running:
  (slick/init)
  (process-send-string slick/process (concat (json-encode command) "\n"))
  (push (or callback '(lambda () nil)) slick/callbacks))

(defun slick/command (command args &optional callback)
  "Send a command as a list containing the name and
arguments. Sleeps for 50ms after sending to let the output get
processed."
  (slick/log 'slick/input (json-encode `(,command . ,args)))
  (slick/send `(,command . ,args) callback))

(defun slick/client-state ()
  "Returns the current file and cursor position."
  `(:path ,(buffer-file-name)
    :cursorPos (,(line-number-at-pos) ,(+ (current-column) 1))))

(defun slick/enter-hole ()
  "Enters the hole at the current cursor position."
  (interactive)
  (slick/command "EnterHole" (list (slick/client-state))))

(defun slick/next-hole ()
  "Jumps to and enters the next hole position and enters that
hole, if any."
  (interactive)
  (slick/load)
  (slick/command "NextHole" (list (slick/client-state)) (indirect-function 'slick/enter-hole)))

(defun slick/prev-hole ()
  "Jumps to and enters the previous hole position and enters that
hole, if any."
  (interactive)
  (slick/load)
  (slick/command "PrevHole" (list (slick/client-state)))
  (slick/enter-hole))

(defun slick/get-env ()
  "Gets the type of the currently entered hole and any relevant
bindings in its scope."
  (interactive)
  (slick/command "GetEnv" (list (slick/client-state))))

(defun slick/case-further (identifier)
  (slick/command "CaseFurther" identifier (list (slick/client-state))))

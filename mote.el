(require 'json)

(defvar mote/program "mote")
(defvar mote/process nil)
(defvar mote/info-buffer-name "*mote*")

(defvar mote/callbacks '())

(defvar mote/default-options '(:sendOutputAsData :json-false :withSuggestions t))

(defun mote/write-to-process-buffer (string)
  "Writes the given string to the process buffer."
  (let ((proc mote/process))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun mote/display-buffer (buffer-or-name)
  "Display the given buffer in another window without selecting
it. If the buffer doesn't exist, create it."
  (let ((buffer (get-buffer-create buffer-or-name)))
    (display-buffer buffer)))

(defun mote/set-info-window (string)
  "If the input is multiple lines, replaces the contents of the
*mote* buffer with the given string and pops to it. Otherwise
just echoes it in minibuffer."
  (if (string-match ".*\n.*" string)
   (progn (mote/display-buffer mote/info-buffer-name)
          (with-current-buffer mote/info-buffer-name
            (erase-buffer)
            (insert string)
            (beginning-of-buffer)))
   (message string)))

(defun mote/insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    ;; Process output line by line:
    (dolist (line (split-string string "\n" t))
      (unless (equal line "")
        (let* ((json-array-type 'list)
               (output (json-read-from-string line)))
          (unless (equal output '("Ok"))
            (mote/execute output))
          (mote/write-to-process-buffer (concat line "\n"))))
      (when mote/callbacks
        (let ((fun (last mote/callbacks)))
          (when fun
            (funcall (car fun))
            (setq mote/callbacks (butlast mote/callbacks))))))))

(defun mote/init ()
  "Starts the mote process if it isn't already running."
  (interactive)
  (setq mote/callbacks '())
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
     (mote/set-info-window text))
    (`("Error" "nohole")
     (message "No current hole."))
    (`("Replace" (,start ,end) ,file ,contents)
     (mote/replace-region (mote/parse-pos start) (mote/parse-pos end) file contents))
    (`("Insert" ,start ,file ,contents)
     (let ((end (list (car start) (+ (cadr start) 1))))
       (mote/replace-region (mote/parse-pos start) (mote/parse-pos end) file contents)))
    (x (message (format "%s" x)))))

(defun mote/parse-pos (pos)
  "Given a pair containing a line and column number, returns an
Emacs position.

Subtracts 1 from the column to account for different indexing
compared to mote itself."
  (save-excursion
    (goto-line (car pos))
    (move-to-column (- (cadr pos) 1))
    (point)))

(defun mote/replace-region (start end file contents)
  "Given Emacs buffer positions and a file, replace the region
between the positions with the given content text."
  (save-excursion
    (goto-char start)
    (delete-region start end)
    (insert contents)))

(defun mote/load (&optional callback)
  (interactive)
  "Loads the current file into the running mote process."
  (save-buffer)
  (push (or callback '(lambda () nil)) mote/callbacks)
  (let ((command (concat (json-encode `("Load" ,(buffer-file-name))) "\n")))
    (mote/write-to-process-buffer (concat "> " command))
    (process-send-string mote/process command)))

(defun mote/send (command &optional callback)
  "Send the given string to the active mote process. Before
sending the actual command, ensure the process is running and
loads the current file."
  ;; Make sure the process is up and running:
  (mote/init)
  (mote/write-to-process-buffer (concat "> " (json-encode command) "\n"))
  (push (or callback '(lambda () nil)) mote/callbacks)
  (process-send-string mote/process (concat (json-encode command) "\n")))

(defun mote/command (command args &optional callback)
  "Send a command as a list containing the name and
arguments. Sleeps for 50ms after sending to let the output get
processed."
  (mote/send `(,command . ,args) callback))

(defun mote/client-state ()
  "Returns the current file and cursor position."
  (let ((col (- (point) (line-beginning-position))))
    `(:path ,(buffer-file-name)
      :cursorPos (,(line-number-at-pos) ,(+ col 1)))))

(defun mote/enter-hole ()
  "Enters the hole at the current cursor position."
  (interactive)
  (mote/command "EnterHole" (list (mote/client-state))))

(defun mote/next-hole ()
  "Jumps to and enters the next hole position and enters that
hole, if any."
  (interactive)
  (mote/load
   (lambda () (mote/command "NextHole" (list (mote/client-state))
     (indirect-function 'mote/enter-hole)))))

(defun mote/prev-hole ()
  "Jumps to and enters the previous hole position and enters that
hole, if any."
  (interactive)
  (mote/load
   (lambda ()
     (mote/command "PrevHole" (list (mote/client-state))
       (indirect-function 'mote/enter-hole)))))

(defun mote/hole-info ()
  "Gets the type of the currently entered hole and any relevant
bindings in its scope."
  (interactive)
  (mote/command "GetHoleInfo" (list (mote/client-state) mote/default-options)))

(defun mote/case-further (identifier)
  "Case expands the given identifier."
  (mote/command "CaseFurther" (list identifier (mote/client-state))))

(defun mote/identifier-at-point ()
  "Gets the Haskell identifier at the current point, if any. This
is basically a custom version of `word-at-point' which accounts
for things like primes (x')."
  (save-excursion
    (let ((start (progn (forward-char) (re-search-backward "\\_<") (point)))
          (end (progn (re-search-forward "\\_>") (point))))
      (buffer-substring-no-properties start end))))

(defun mote/refine (name)
  (interactive "sExpression:")
  "Refines the hole by applying the given function to it."
  (mote/command "Refine" (list name (mote/client-state))))

(defun mote/case-at-point ()
  "Tries to case expand the identifier at point, if possible."
  (interactive)
  (mote/case-further (mote/identifier-at-point)))

(defun mote/case-on (name)
  "Tries to insert a case statement matching on the given expression."
  (interactive "sExpression to match: ")
  (mote/command "CaseOn" (list name (mote/client-state))))

(defun mote/default-keybindings ()
  "Sets the default keybindings for the mote commands."
  (local-set-key (kbd "C-c C-n") 'mote/next-hole)
  (local-set-key (kbd "C-c C-p") 'mote/prev-hole)
  (local-set-key (kbd "C-c C-e") 'mote/enter-hole)
  (local-set-key (kbd "C-c C-/") 'mote/hole-info)
  (local-set-key (kbd "C-c C-f") 'mote/case-at-point)
  (local-set-key (kbd "C-c C-o") 'mote/case-on)
  (local-set-key (kbd "C-c <C-return>") 'mote/refine))

(provide 'mote)

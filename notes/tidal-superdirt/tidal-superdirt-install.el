;;; vivid-tidal-superdirt-install.el --- Utilities to install SuperDirt -*- lexical-binding: t -*-
;;
;; Filename: vivid-tidal-superdirt-install.el
;; Description: Utilities to install SuperDirt from Emacs
;; Author: Numa Tortolero
;; Maintainer: Numa Tortolero
;; Created: Sat Feb  7 15:03:58 2026 (-0400)
;; Version: 0.0.0
;; Package-Requires: (sclang)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Utilities to install SuperDirt and scel (sclang-mode).
;;      Installation:x
;;      Ensure you have sclang (SuperCollider client) installed.
;;
;;    Usage:
;;      From Emacs: M-x tidal-install-scel
;;                  M-x tidal-install-superdirt
;;
;;      Or from Emacs intend: M-x tidal-install-scel-dirt
;;
;;      This last will intend install scel and superdirt.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'osc)
(require 'tidal-osc)

(defgroup vivid-tidal-superdirt-install nil
  "Installation utilities for SuperDirt and scel."
  :group 'tidal)

(defcustom tidal-superdirt-install-sclang-path (or (executable-find "sclang") "sclang")
  "Path to sclang SuperCollider client."
  :type 'string
  :group 'vivid-tidal-superdirt-install)

(defcustom vivid-tidal-superdirt-install-emacs-port 57130
  "Port where Emacs listens for sclang responses."
  :type 'integer
  :group 'vivid-tidal-superdirt-install)

(defcustom vivid-tidal-superdirt-install-sclang-port 57120
  "Port where sclang listens for Emacs commands."
  :type 'integer
  :group 'vivid-tidal-superdirt-install)

(defvar vivid-tidal-superdirt-install--osc-server nil
  "Internal OSC server process for receiving replies from sclang.")

(defvar vivid-tidal-superdirt-install-dirt-quark "SuperDirt")

(defvar vivid-tidal-superdirt-install-scel-quark "https://github.com/supercollider/scel")

(defvar vivid-tidal-superdirt-install-buffer "*SCLang:Command*"
  "Name of the buffer used for SuperDirt installation output.")

(defun vivid-tidal-superdirt-install-get-penline (command &rest args)
  "Execute COMMAND with ARGS synchronously and return the penultimate line."
  (with-temp-buffer
    (let ((exit-code (apply 'call-process command nil t nil args)))
      (goto-char (point-min))
      (let* ((content (buffer-string))
              (lines (split-string content "\n" t)))
        (if (>= (length lines) 2)
          (nth (- (length lines) 2) lines)
          nil)))))

(defconst vivid-tidal-superdirt-install--spinner-chars ["|" "/" "-" "\\"]
  "Characters used for the spinner animation in the mode-line.")

(defun vivid-tidal-superdirt-install--update-spinner (proc)
  "Update the spinner in the mode-line for PROC."
  (when (process-live-p proc)
    (with-current-buffer (process-buffer proc)
      (setq tidal-install--spinner-state (% (1+ tidal-install--spinner-state) 4))
      (setq mode-line-process
        (format " [%s] %s"
          (aref tidal-install--spinner-chars tidal-install--spinner-state)
          (process-status proc)))
      (force-mode-line-update))))

(defun vivid-tidal-superdirt-install-process-filter (proc string)
  "Process PROC filter for process tidal installation output STRING.
Updates the buffer with output from proc and string, and updates the spinner."
  (vivid-tidal-superdirt-install--update-spinner proc)
  (let ((buffer (process-buffer proc)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
               (moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (dolist (c (append string nil))
              (cond
                ((= c ?\r)
                  (goto-char (line-beginning-position))
                  (set-marker (process-mark proc) (point)))
                ((= c ?\n)
                  (goto-char (point-max))
                  (insert "\n")
                  (set-marker (process-mark proc) (point)))
                (t
                  (goto-char (process-mark proc))
                  (unless (eolp) (delete-char 1))
                  (insert c)
                  (set-marker (process-mark proc) (point))))))
          (ansi-color-apply-on-region (line-beginning-position) (point-max))
          (when moving
            (goto-char (process-mark proc))
            (dolist (win (get-buffer-window-list buffer nil t))
              (set-window-point win (process-mark proc)))))))))

(defun vivid-tidal-superdirt-install-run (command args)
  "Start the process to install SuperDirt calling COMMAND with ARGS."
  (let ((proc (start-process
                "SCLang:Command"
                vivid-tidal-superdirt-install-buffer
                command
                args)))
    (when proc
      (set-process-filter proc 'vivid-tidal-superdirt-install-process-filter)
      (display-buffer vivid-tidal-superdirt-install-buffer
        '((display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 0.25)
           (side . bottom))))
    proc))

(defun vivid-tidal-superdirt-install-sclang-command (command)
  "Execute a SuperCollider COMMAND in the sclang interpreter."
  (let ((vivid-tidal-superdirt-install-tmp-file
          (make-temp-file "vivid-tidal-superdirt-install-" nil nil command)))
    (vivid-tidal-superdirt-install-run
      (format "%s" vivid-tidal-superdirt-install-sclang-path)
      (format "%s" vivid-tidal-superdirt-install-tmp-file))))

(defvar vivid-tidal-superdirt-install-quarks-folder nil
  "Path to the SuperCollider Quarks folder.")

(defun vivid-tidal-superdirt-install-get-result-sync (code)
  "Execute CODE in sclang synchronously and return the result as a string."
  (let ((tmp (make-temp-file "tidal-sclang-" nil ".scd" (format "(%s).postln;0.exit;" code))))
    (unwind-protect
      (vivid-tidal-superdirt-install-get-penline vivid-tidal-superdirt-install-sclang-path tmp)
      (delete-file tmp))))

(defun vivid-tidal-superdirt-install-quarks-get-dir ()
  "Get the SuperCollider Quarks folder synchronously."
  (vivid-tidal-superdirt-install-get-result-sync "Quarks.folder"))

(defun vivid-tidal-superdirt-install--init-quarks-folder ()
  "Initialize the quarks folder path if not already set or if it's invalid."
  (unless (stringp vivid-tidal-superdirt-install-quarks-folder)
    (setq vivid-tidal-superdirt-install-quarks-folder
      (let ((dir (vivid-tidal-superdirt-install-quarks-get-dir)))
        (if (and (stringp dir) (file-directory-p dir))
          dir
          (expand-file-name
            "~/.local/share/SuperCollider/downloaded-quarks"))))))

(defvar vivid-tidal-superdirt-install-scel-path nil
  "Path to the SuperCollider Emacs scel quark folder.")

(defvar vivid-tidal-superdirt-install-path nil
  "Path to the SuperDirt quark file.")

(defun vivid-tidal-superdirt-install--update-paths ()
  "Update scel and superdirt paths based on the quarks folder."
  (vivid-tidal-superdirt-install--init-quarks-folder)
  (setq vivid-tidal-superdirt-install-scel-path
    (expand-file-name "scel/el" vivid-tidal-superdirt-install-quarks-folder))
  (setq vivid-tidal-superdirt-install-path
    (expand-file-name "SuperDirt/SuperDirt.quark" vivid-tidal-superdirt-install-quarks-folder)))

(defvar vivid-tidal-superdirt-install--bridge-ready nil
  "Internal flag indicating if the SC bridge is ready to receive commands.")

(defvar vivid-tidal-superdirt-install--pending-commands nil
  "List of commands waiting for the bridge to be ready.")

(defun vivid-tidal-superdirt-install-osc-handler (path &rest args)
  "Handle OSC messages from sclang."
  (pcase path
    ("/dirt/quarksdir"
      (setq vivid-tidal-superdirt-install-quarks-folder (car args))
      (vivid-tidal-superdirt-install--update-paths)
      (message "✅ Quarks folder updated: %s" (car args)))
    ("/dirt/installed"
      (message "✅ Quark installed: %s" (car args)))
    ("/dirt/status"
      (let ((msg (car args)))
        (message "📢 SC: %s" msg)
        ;; Detect if this is the bridge ready message
        (when (string-match-p "OSC Bridge active" msg)
          (setq vivid-tidal-superdirt-install--bridge-ready t)
          (vivid-tidal-superdirt-install--process-pending-commands))))
    ("/dirt/ready"
      (message "🚀 SuperDirt is ready! Starting Haskell boot sequence...")
      (when (fboundp 'vivid-tidal-start-run)
        (vivid-tidal-start-run)))))

(defun vivid-tidal-superdirt-install--process-pending-commands ()
  "Execute all commands in the pending queue safely, handling both old and new formats."
  (let ((cmds (reverse vivid-tidal-superdirt-install--pending-commands)))
    (setq vivid-tidal-superdirt-install--pending-commands nil)
    (when cmds
      (message "📩 Processing %d pending commands..." (length cmds))
      (dolist (cmd cmds)
        (condition-case err
          (cond
            ((and (listp cmd) (functionp (car cmd)))
              (apply (car cmd) (cdr cmd)))
            ((functionp cmd)
              (funcall cmd))
            (t (message "⚠️ Skipping invalid command: %S" cmd)))
          (error (message "❌ Error processing command: %s" (error-message-string err))))))))

(defun vivid-tidal-superdirt-install-enqueue-command (func &rest args)
  "Add FUNC and ARGS to the pending queue and ensure bridge is starting."
  (push (cons func args) vivid-tidal-superdirt-install--pending-commands)
  (let ((proc (get-process "SCLang:Command")))
    (if (and proc (process-live-p proc))
      (message "⏳ Waiting for SCLang to be ready...")
      (message "🚀 Starting SCLang bridge automatically...")
      (vivid-tidal-superdirt-install-start-bridge))))

(defun vivid-tidal-superdirt-install-ensure-osc-server ()
  "Ensure the OSC server is running on the default port."
  (unless (and vivid-tidal-superdirt-install--osc-server
            (process-live-p vivid-tidal-superdirt-install--osc-server))
    (setq vivid-tidal-superdirt-install--osc-server
      (osc-make-server "127.0.0.1" vivid-tidal-superdirt-install-emacs-port
        #'vivid-tidal-superdirt-install-osc-handler))))

(defconst vivid-tidal-superdirt-install-bridge-sc
  "(
var emacs = NetAddr(\"127.0.0.1\", %d);
thisProcess.openUDPPort(%d);

OSCdef(\\getquarksdir, { |msg|
    emacs.sendMsg(\"/dirt/quarksdir\", Quarks.folder);
}, '/dirt/getquarksdir');

OSCdef(\\installquark, { |msg|
    var name = msg[1].asString;
    \"[SC] Installing quark: %%\".format(name).postln;
    {
        Quarks.install(name);
        emacs.sendMsg(\"/dirt/installed\", name);
    }.fork(AppClock);
}, '/dirt/install');

OSCdef(\\eval, { |msg|
    var code = msg[1].asString;
    var result = code.interpret;
    emacs.sendMsg(\"/dirt/status\", \"Result: %%\".format(result));
}, '/dirt/eval');

{
    var msg = \"🚀 [SC] OSC Bridge active on port %d. Ready for commands.\";
    msg.postln;
    emacs.sendMsg(\"/dirt/status\", msg);
}.value;

{ inf.wait }.fork;
)
"
  "SuperCollider code for the OSC bridge.")

(defun vivid-tidal-superdirt-install-osc-eval (code)
  "Evaluate CODE in sclang via OSC bridge (bridge will start if needed)."
  (interactive "sSC Code: ")
  (vivid-tidal-superdirt-install-show-buffer)
  (if vivid-tidal-superdirt-install--bridge-ready
    (let ((client (osc-make-client "127.0.0.1" vivid-tidal-superdirt-install-sclang-port)))
      (osc-send-message client "/dirt/eval" code))
    (vivid-tidal-superdirt-install-enqueue-command #'vivid-tidal-superdirt-install-osc-eval code)))

(defun vivid-tidal-superdirt-install-start-bridge ()
  "Start sclang with the OSC bridge."
  (interactive)
  (setq vivid-tidal-superdirt-install--bridge-ready nil) ; Reset flag when starting
  (vivid-tidal-superdirt-install-ensure-osc-server)
  (vivid-tidal-superdirt-install-sclang-command
    (format vivid-tidal-superdirt-install-bridge-sc
      vivid-tidal-superdirt-install-emacs-port
      vivid-tidal-superdirt-install-sclang-port
      vivid-tidal-superdirt-install-sclang-port)))

(defun vivid-tidal-superdirt-install-query-quarks-dir ()
  "Query the Quarks directory via OSC (bridge will start if needed)."
  (interactive)
  (if vivid-tidal-superdirt-install--bridge-ready
    (let ((client (osc-make-client "127.0.0.1" vivid-tidal-superdirt-install-sclang-port)))
      (osc-send-message client "/dirt/getquarksdir"))
    (vivid-tidal-superdirt-install-enqueue-command #'vivid-tidal-superdirt-install-query-quarks-dir)))

(defun vivid-tidal-superdirt-install-sclang-mode-exists-p ()
  "Return t if `sclang-mode' is in your `load-path'."
  (require 'sclang nil t))

(defun vivid-tidal-superdirt-install-exists-p ()
  "Return t if Emacs can find the SuperDirt quark."
  (vivid-tidal-superdirt-install--update-paths)
  (and vivid-tidal-superdirt-install-path (file-exists-p vivid-tidal-superdirt-install-path)))

(defun vivid-tidal-superdirt-install-show-buffer ()
  "Ensure the SCLang command buffer is visible with the requested 25%% height."
  (interactive)
  (let ((buffer (get-buffer-create vivid-tidal-superdirt-install-buffer)))
    (display-buffer buffer
      '((display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.25)
         (side . bottom)))))

(defun vivid-tidal-superdirt-install-quark (quark)
  "Install the specified SuperCollider QUARK, using the bridge if possible."
  (vivid-tidal-superdirt-install-show-buffer)
  (if vivid-tidal-superdirt-install--bridge-ready
    (let ((client (osc-make-client "127.0.0.1" vivid-tidal-superdirt-install-sclang-port)))
      (message "📤 Sending install request for %s..." quark)
      (osc-send-message client "/dirt/install" quark))
    (vivid-tidal-superdirt-install-enqueue-command #'vivid-tidal-superdirt-install-quark quark)))

;;;###autoload
(defun vivid-tidal-superdirt-install-scel ()
  "Install `sclang-mode' as a SuperCollider quark."
  (interactive)
  (vivid-tidal-superdirt-install--update-paths)
  (unless (and vivid-tidal-superdirt-install-scel-path (file-exists-p vivid-tidal-superdirt-install-scel-path))
    (message "🔧 Installing scel (sclang-mode)...")
    (vivid-tidal-superdirt-install-quark vivid-tidal-superdirt-install-scel-quark)
    (when (and vivid-tidal-superdirt-install-scel-path (file-exists-p vivid-tidal-superdirt-install-scel-path))
      (add-to-list 'load-path vivid-tidal-superdirt-install-scel-path))))

;;;###autoload
(defun vivid-tidal-superdirt-install-dirt ()
  "Install the SuperDirt quark."
  (interactive)
  (vivid-tidal-superdirt-install--update-paths)
  (unless (vivid-tidal-superdirt-install-exists-p)
    (message "🔧 Installing SuperDirt quark...")
    (vivid-tidal-superdirt-install-quark vivid-tidal-superdirt-install-dirt-quark)))

;;;###autoload
(defun vivid-tidal-superdirt-install ()
  "Install the scel Elisp package and SuperDirt quark."
  (interactive)
  (vivid-tidal-superdirt-install-scel)
  (vivid-tidal-superdirt-install-dirt))

;;;

(defun vivid-tidal-superdirt-install-stop-bridge ()
  "Stop the sclang bridge and the OSC server."
  (interactive)
  (setq vivid-tidal-superdirt-install--bridge-ready nil)
  (when (and vivid-tidal-superdirt-install--osc-server
          (process-live-p vivid-tidal-superdirt-install--osc-server))
    (delete-process vivid-tidal-superdirt-install--osc-server)
    (setq vivid-tidal-superdirt-install--osc-server nil)
    (message "🛑 OSC Server stopped."))
  (let ((proc (get-process "SCLang:Command")))
    (when (and proc (process-live-p proc))
      (kill-process proc)
      (message "🛑 SCLang Bridge process killed."))))

(defun vivid-tidal-superdirt-install-quit-buffer ()
  "Close the Tidal installation buffer if it exists and delete its window."
  (interactive)
  (vivid-tidal-superdirt-install-stop-bridge)
  (let ((buffer (get-buffer vivid-tidal-superdirt-install-buffer)))
    (and buffer
      (let ((window (get-buffer-window buffer))
             (process (get-buffer-process buffer)))
        (when process (process-live-p process) (kill-process process))
        (and window (quit-window t window))
        (and buffer (buffer-live-p buffer) (kill-buffer buffer))))))
(defalias 'quit-install 'vivid-tidal-superdirt-install-quit-buffer)

(defun vivid-tidal-superdirt-install-clean ()
  "Uninstall SuperDirt and scel quarks cleanly via SC to update compilation paths."
  (interactive)
  (vivid-tidal-superdirt-install--update-paths)
  (if vivid-tidal-superdirt-install--bridge-ready
    (progn
      (message "🧹 Sending uninstall requests via OSC bridge...")
      (vivid-tidal-superdirt-install-osc-eval
        "Quarks.uninstall(\"SuperDirt\"); Quarks.uninstall(\"scel\");"))
    (message "🧹 Cleaning quarks via one-shot sclang command...")
    (vivid-tidal-superdirt-install-sclang-command
      "Quarks.uninstall(\"SuperDirt\"); Quarks.uninstall(\"scel\"); 0.exit;"))
  (message "✨ Cleanup initiated. SuperCollider will update its compilation paths."))

;; (sclang-send-string "\n" "Quarks.uninstall(\"superfomus\");")

(provide 'vivid-tidal-superdirt-install)
;;; vivid-tidal-superdirt-install.el ends here

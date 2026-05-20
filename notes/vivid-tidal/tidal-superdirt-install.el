;;; tidal-superdirt-install.el --- Utilities to install SuperDirt -*- lexical-binding: t -*-
;;
;; Filename: tidal-superdirt-install.el
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

(defgroup tidal-superdirt-install nil
  "Installation utilities for SuperDirt and scel."
  :group 'tidal)

(defcustom tidal-superdirt-install-sclang-path (or (executable-find "sclang") "sclang")
  "Path to sclang SuperCollider client."
  :type 'string
  :group 'tidal-superdirt-install)

(defcustom tidal-superdirt-install-emacs-port 57130
  "Port where Emacs listens for sclang responses."
  :type 'integer
  :group 'tidal-superdirt-install)

(defcustom tidal-superdirt-install-sclang-port 57120
  "Port where sclang listens for Emacs commands."
  :type 'integer
  :group 'tidal-superdirt-install)

(defvar tidal-superdirt-install--osc-server nil
  "Internal OSC server process for receiving replies from sclang.")

(defvar tidal-superdirt-install-dirt-quark "SuperDirt")

(defvar tidal-superdirt-install-scel-quark "https://github.com/supercollider/scel")

(defvar tidal-superdirt-install-buffer "*SCLang:Command*"
  "Name of the buffer used for SuperDirt installation output.")

(defun tidal-superdirt-install-get-penline (command &rest args)
  "Execute COMMAND with ARGS synchronously and return the penultimate line."
  (with-temp-buffer
    (let ((exit-code (apply 'call-process command nil t nil args)))
      (goto-char (point-min))
      (let* ((content (buffer-string))
              (lines (split-string content "\n" t)))
        (if (>= (length lines) 2)
          (nth (- (length lines) 2) lines)
          nil)))))

(defconst tidal-superdirt-install--spinner-chars ["|" "/" "-" "\\"]
  "Characters used for the spinner animation in the mode-line.")

(defun tidal-superdirt-install--update-spinner (proc)
  "Update the spinner in the mode-line for PROC."
  (when (process-live-p proc)
    (with-current-buffer (process-buffer proc)
      (setq tidal-install--spinner-state (% (1+ tidal-install--spinner-state) 4))
      (setq mode-line-process
        (format " [%s] %s"
          (aref tidal-install--spinner-chars tidal-install--spinner-state)
          (process-status proc)))
      (force-mode-line-update))))

(defun tidal-superdirt-install-process-filter (proc string)
  "Process PROC filter for process tidal installation output STRING.
Updates the buffer with output from proc and string, and updates the spinner."
  (tidal-superdirt-install--update-spinner proc)
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

(defun tidal-superdirt-install-run (command args)
  "Start the process to install SuperDirt calling COMMAND with ARGS."
  (let ((proc (start-process
                "SCLang:Command"
                tidal-superdirt-install-buffer
                command
                args)))
    (when proc
      (set-process-filter proc 'tidal-superdirt-install-process-filter)
      (display-buffer tidal-superdirt-install-buffer
        '((display-buffer-reuse-window display-buffer-at-bottom)
           (window-height . 0.25)
           (side . bottom))))
    proc))

(defun tidal-superdirt-install-sclang-command (command)
  "Execute a SuperCollider COMMAND in the sclang interpreter."
  (let ((tidal-superdirt-install-tmp-file
          (make-temp-file "tidal-superdirt-install-" nil nil command)))
    (tidal-superdirt-install-run
      (format "%s" tidal-superdirt-install-sclang-path)
      (format "%s" tidal-superdirt-install-tmp-file))))

(defvar tidal-superdirt-install-quarks-folder nil
  "Path to the SuperCollider Quarks folder.")

(defun tidal-superdirt-install-get-result-sync (code)
  "Execute CODE in sclang synchronously and return the result as a string."
  (let ((tmp (make-temp-file "tidal-sclang-" nil ".scd" (format "(%s).postln;0.exit;" code))))
    (unwind-protect
      (tidal-superdirt-install-get-penline tidal-superdirt-install-sclang-path tmp)
      (delete-file tmp))))

(defun tidal-superdirt-install-quarks-get-dir ()
  "Get the SuperCollider Quarks folder synchronously."
  (tidal-superdirt-install-get-result-sync "Quarks.folder"))

(defun tidal-superdirt-install--init-quarks-folder ()
  "Initialize the quarks folder path if not already set or if it's invalid."
  (unless (stringp tidal-superdirt-install-quarks-folder)
    (setq tidal-superdirt-install-quarks-folder
      (let ((dir (tidal-superdirt-install-quarks-get-dir)))
        (if (and (stringp dir) (file-directory-p dir))
          dir
          (expand-file-name
            "~/.local/share/SuperCollider/downloaded-quarks"))))))

(defvar tidal-superdirt-install-scel-path nil
  "Path to the SuperCollider Emacs scel quark folder.")

(defvar tidal-superdirt-install-path nil
  "Path to the SuperDirt quark file.")

(defun tidal-superdirt-install--update-paths ()
  "Update scel and superdirt paths based on the quarks folder."
  (tidal-superdirt-install--init-quarks-folder)
  (setq tidal-superdirt-install-scel-path
    (expand-file-name "scel/el" tidal-superdirt-install-quarks-folder))
  (setq tidal-superdirt-install-path
    (expand-file-name "SuperDirt/SuperDirt.quark" tidal-superdirt-install-quarks-folder)))

(defvar tidal-superdirt-install--bridge-ready nil
  "Internal flag indicating if the SC bridge is ready to receive commands.")

(defvar tidal-superdirt-install--pending-commands nil
  "List of commands waiting for the bridge to be ready.")

(defun tidal-superdirt-install-osc-handler (path &rest args)
  "Handle OSC messages from sclang."
  (pcase path
    ("/dirt/quarksdir"
      (setq tidal-superdirt-install-quarks-folder (car args))
      (tidal-superdirt-install--update-paths)
      (message "✅ Quarks folder updated: %s" (car args)))
    ("/dirt/installed"
      (message "✅ Quark installed: %s" (car args)))
    ("/dirt/status"
      (let ((msg (car args)))
        (message "📢 SC: %s" msg)
        ;; Detect if this is the bridge ready message
        (when (string-match-p "OSC Bridge active" msg)
          (setq tidal-superdirt-install--bridge-ready t)
          (tidal-superdirt-install--process-pending-commands))))
    ("/dirt/ready"
      (message "🚀 SuperDirt is ready! Starting Haskell boot sequence...")
      (when (fboundp 'vivid-tidal-start-run)
        (vivid-tidal-start-run)))))

(defun tidal-superdirt-install--process-pending-commands ()
  "Execute all commands in the pending queue safely, handling both old and new formats."
  (let ((cmds (reverse tidal-superdirt-install--pending-commands)))
    (setq tidal-superdirt-install--pending-commands nil)
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

(defun tidal-superdirt-install-enqueue-command (func &rest args)
  "Add FUNC and ARGS to the pending queue and ensure bridge is starting."
  (push (cons func args) tidal-superdirt-install--pending-commands)
  (let ((proc (get-process "SCLang:Command")))
    (if (and proc (process-live-p proc))
      (message "⏳ Waiting for SCLang to be ready...")
      (message "🚀 Starting SCLang bridge automatically...")
      (tidal-superdirt-install-start-bridge))))

(defun tidal-superdirt-install-ensure-osc-server ()
  "Ensure the OSC server is running on the default port."
  (unless (and tidal-superdirt-install--osc-server
            (process-live-p tidal-superdirt-install--osc-server))
    (setq tidal-superdirt-install--osc-server
      (osc-make-server "127.0.0.1" tidal-superdirt-install-emacs-port
        #'tidal-superdirt-install-osc-handler))))

(defconst tidal-superdirt-install-bridge-sc
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

(defun tidal-superdirt-install-osc-eval (code)
  "Evaluate CODE in sclang via OSC bridge (bridge will start if needed)."
  (interactive "sSC Code: ")
  (tidal-superdirt-install-show-buffer)
  (if tidal-superdirt-install--bridge-ready
    (let ((client (osc-make-client "127.0.0.1" tidal-superdirt-install-sclang-port)))
      (osc-send-message client "/dirt/eval" code))
    (tidal-superdirt-install-enqueue-command #'tidal-superdirt-install-osc-eval code)))

(defun tidal-superdirt-install-start-bridge ()
  "Start sclang with the OSC bridge."
  (interactive)
  (setq tidal-superdirt-install--bridge-ready nil) ; Reset flag when starting
  (tidal-superdirt-install-ensure-osc-server)
  (tidal-superdirt-install-sclang-command
    (format tidal-superdirt-install-bridge-sc
      tidal-superdirt-install-emacs-port
      tidal-superdirt-install-sclang-port
      tidal-superdirt-install-sclang-port)))

(defun tidal-superdirt-install-query-quarks-dir ()
  "Query the Quarks directory via OSC (bridge will start if needed)."
  (interactive)
  (if tidal-superdirt-install--bridge-ready
    (let ((client (osc-make-client "127.0.0.1" tidal-superdirt-install-sclang-port)))
      (osc-send-message client "/dirt/getquarksdir"))
    (tidal-superdirt-install-enqueue-command #'tidal-superdirt-install-query-quarks-dir)))

(defun tidal-superdirt-install-sclang-mode-exists-p ()
  "Return t if `sclang-mode' is in your `load-path'."
  (require 'sclang nil t))

(defun tidal-superdirt-install-exists-p ()
  "Return t if Emacs can find the SuperDirt quark."
  (tidal-superdirt-install--update-paths)
  (and tidal-superdirt-install-path (file-exists-p tidal-superdirt-install-path)))

(defun tidal-superdirt-install-show-buffer ()
  "Ensure the SCLang command buffer is visible with the requested 25%% height."
  (interactive)
  (let ((buffer (get-buffer-create tidal-superdirt-install-buffer)))
    (display-buffer buffer
      '((display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.25)
         (side . bottom)))))

(defun tidal-superdirt-install-quark (quark)
  "Install the specified SuperCollider QUARK, using the bridge if possible."
  (tidal-superdirt-install-show-buffer)
  (if tidal-superdirt-install--bridge-ready
    (let ((client (osc-make-client "127.0.0.1" tidal-superdirt-install-sclang-port)))
      (message "📤 Sending install request for %s..." quark)
      (osc-send-message client "/dirt/install" quark))
    (tidal-superdirt-install-enqueue-command #'tidal-superdirt-install-quark quark)))

;;;###autoload
(defun tidal-superdirt-install-scel ()
  "Install `sclang-mode' as a SuperCollider quark."
  (interactive)
  (tidal-superdirt-install--update-paths)
  (unless (and tidal-superdirt-install-scel-path (file-exists-p tidal-superdirt-install-scel-path))
    (message "🔧 Installing scel (sclang-mode)...")
    (tidal-superdirt-install-quark tidal-superdirt-install-scel-quark)
    (when (and tidal-superdirt-install-scel-path (file-exists-p tidal-superdirt-install-scel-path))
      (add-to-list 'load-path tidal-superdirt-install-scel-path))))

;;;###autoload
(defun tidal-superdirt-install-dirt ()
  "Install the SuperDirt quark."
  (interactive)
  (tidal-superdirt-install--update-paths)
  (unless (tidal-superdirt-install-exists-p)
    (message "🔧 Installing SuperDirt quark...")
    (tidal-superdirt-install-quark tidal-superdirt-install-dirt-quark)))

;;;###autoload
(defun tidal-superdirt-install ()
  "Install the scel Elisp package and SuperDirt quark."
  (interactive)
  (tidal-superdirt-install-scel)
  (tidal-superdirt-install-dirt))

;;;

(defun tidal-superdirt-install-stop-bridge ()
  "Stop the sclang bridge and the OSC server."
  (interactive)
  (setq tidal-superdirt-install--bridge-ready nil)
  (when (and tidal-superdirt-install--osc-server
          (process-live-p tidal-superdirt-install--osc-server))
    (delete-process tidal-superdirt-install--osc-server)
    (setq tidal-superdirt-install--osc-server nil)
    (message "🛑 OSC Server stopped."))
  (let ((proc (get-process "SCLang:Command")))
    (when (and proc (process-live-p proc))
      (kill-process proc)
      (message "🛑 SCLang Bridge process killed."))))

(defun tidal-superdirt-install-quit-buffer ()
  "Close the Tidal installation buffer if it exists and delete its window."
  (interactive)
  (tidal-superdirt-install-stop-bridge)
  (let ((buffer (get-buffer tidal-superdirt-install-buffer)))
    (and buffer
      (let ((window (get-buffer-window buffer))
             (process (get-buffer-process buffer)))
        (when process (process-live-p process) (kill-process process))
        (and window (quit-window t window))
        (and buffer (buffer-live-p buffer) (kill-buffer buffer))))))
(defalias 'quit-install 'tidal-superdirt-install-quit-buffer)

(defun tidal-superdirt-install-clean ()
  "Uninstall SuperDirt and scel quarks cleanly via SC to update compilation paths."
  (interactive)
  (tidal-superdirt-install--update-paths)
  (if tidal-superdirt-install--bridge-ready
    (progn
      (message "🧹 Sending uninstall requests via OSC bridge...")
      (tidal-superdirt-install-osc-eval "Quarks.uninstall(\"SuperDirt\"); Quarks.uninstall(\"scel\");"))
    (message "🧹 Cleaning quarks via one-shot sclang command...")
    (tidal-superdirt-install-sclang-command
      "Quarks.uninstall(\"SuperDirt\"); Quarks.uninstall(\"scel\"); 0.exit;"))
  (message "✨ Cleanup initiated. SuperCollider will update its compilation paths."))

;; (sclang-send-string "\n" "Quarks.uninstall(\"superfomus\");")

(provide 'tidal-superdirt-install)
;;; tidal-superdirt-install.el ends here

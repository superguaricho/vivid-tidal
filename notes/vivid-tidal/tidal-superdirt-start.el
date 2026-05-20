;;; tidal-superdirt-start.el --- SuperDirt initialization for sclang in Emacs -*- lexical-binding: t -*-
;;
;; Filename: tidal-superdirt-start.el
;; Description: SuperDirt initialization for sclang in Emacs
;; Author: Numa Tortolero
;; Maintainer: Numa Tortolero
;; Created: vie ene 23 22:36:06 2026 (-0400)
;; Version: 0.1.0
;; Package-Requires: (Emacs 27.1 sclang osc sclang-ext)
;; URL: https://github.com/superguaricho/tidal
;; Keywords: (Emacs SuperCollider SuperDirt OSC)
;; Compatibility: Emacs 27.1 and later
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;   This Emacs Lisp file provides initialization for SuperDirt
;;   when using SuperCollider (sclang) within Emacs. It includes
;;   functions to start SuperDirt with enhanced memory settings,

;;   set up an OSC listener in Emacs to monitor SuperDirt's status,
;;   and display notifications when SuperCollider and SuperDirt
;;   are ready.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'sclang)
(require 'tidal-osc)
(require 'tidal-superdirt-install)


(declare-function haskell-interactive-switch "haskell")

(add-to-list 'load-path
  (file-name-directory (or load-file-name buffer-file-name)))

(setq sclang-show-workspace-on-startup nil)

(defvar tidal-superdirt-startup-functions nil
  "List of functions to run when SuperDirt starts up.")

;; -----------------------------------------------------------------------------
;; OSC Monitor for SuperDirt
;; -----------------------------------------------------------------------------

(defvar tidal-emacs-osc-server nil)
(defvar tidal-superdirt-boot-hook nil)
(defvar tidal-osc-server nil
  "OSC server process in Emacs for SuperDirt communication.")

(defun tidal-on-superdirt-ready-osc ()
  "Action when Emacs receives OSC from SuperDirt."
  (message "📡 OSCSIGNAL -> SuperDirt is READY! 📡")
  (beep)
  (and tidal-superdirt-startup-functions
    (mapc #'funcall tidal-superdirt-startup-functions)))

(defun tidal-remove-emacs-osc-listener ()
  "Stop the OSC listener in Emacs on port 7777."
  (interactive)
  (and (get-process "OSCserver")
    (ignore-errors (delete-process "OSCserver")))
  (and tidal-osc-server
    (process-status tidal-osc-server)
    (delete-process tidal-osc-server)))

(defun tidal-start-emacs-osc-listener ()
  "Restart the OSC listener in Emacs on port 7777."
  (interactive)
  (tidal-remove-emacs-osc-listener)
  (message "👂 Emacs: Starting OSC listener on port 7777...")
  (setq tidal-osc-server
    (osc-make-server "127.0.0.1" 7777
      (lambda (path &rest args)
        (cond
          ((string= path "/superdirt/ready")
            (run-hooks 'tidal-superdirt-boot-hook)))))))

(add-hook 'tidal-superdirt-boot-hook #'tidal-on-superdirt-ready-osc)

;;;###autoload
(defun tidal-start-superdirt ()
  "Start SuperDirt with extended memory options."
  (interactive)
  (tidal-start-emacs-osc-listener)
  (sclang-eval-string
    "(
s.reboot {
  s.options.numBuffers = 1024 * 256;
  s.options.memSize = 8192 * 32;
  s.options.numWireBufs = 2048;
  s.options.maxNodes = 1024 * 32;
  s.options.numOutputBusChannels = 2;
  s.options.numInputBusChannels = 2;

  s.waitForBoot {
    ~dirt.stop;
    ~dirt = SuperDirt(2, s);
    ~dirt.loadSoundFiles;
    ~dirt.start(57120, 0 ! 12);
    SuperDirt.default = ~dirt;
    s.latency = 0.8;

    // --- Notify Emacs that SuperDirt is ready ---
    NetAddr (\"127.0.0.1\", 57130).sendMsg(\"/dirt/ready\", \"SuperDirt is ready\");
    \"Vivid: SuperDirt is ready. Notifying Emacs...\".postln;
    // ---------------------------------------------

    // --- Vivid: Reload Listener (Robust Version) ---
    OSCFunc({ |msg|
       \"Vivid: Syncing Server and SuperDirt...\".postln;
       fork {
          // 1. Tell the server to load all files from disk
          s.sendMsg(\"/d_loadDir\", \"/home/numa/.local/share/SuperCollider/synthdefs/\");
          s.sync;

          // 2. Tell sclang to read them into its library
          SynthDescLib.global.read;
          0.2.wait;

          // 3. Tell SuperDirt to update its dictionary
          ~dirt.loadSynthDefs;
          \"Vivid: Sync complete. All definitions ready.\".postln;
    }
    }, '/vivid/reload').fix;
  // ---------------------------
  };
  };
  )"))

(add-hook 'sclang-library-startup-hook #'tidal-start-superdirt 95)

(provide 'tidal-superdirt-start)
;;; tidal-superdirt-start.el ends here

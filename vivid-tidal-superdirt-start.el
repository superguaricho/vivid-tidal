;;; vivid-tidal-superdirt-start.el --- SuperDirt initialization for sclang in Emacs -*- lexical-binding: t -*-
;;
;; Filename: vivid-tidal-superdirt-start.el
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
(require 'vivid-tidal-superdirt-install)

(declare-function haskell-interactive-switch "haskell")

(add-to-list 'load-path
  (file-name-directory (or load-file-name buffer-file-name)))

(setq sclang-show-workspace-on-startup nil)

;;;###autoload
(defvar vivid-tidal-superdirt-startup-functions nil
  "List of functions to run when SuperDirt starts up.")

;; -----------------------------------------------------------------------------
;; OSC Monitor for SuperDirt
;; -----------------------------------------------------------------------------

(defvar vivid-tidal-emacs-osc-server nil)
(defvar vivid-tidal-superdirt-boot-hook nil)
(defvar vivid-tidal-osc-server nil
  "OSC server process in Emacs for SuperDirt communication.")

(defun vivid-tidal-on-superdirt-ready-osc ()
  "Action when Emacs receives OSC from SuperDirt."
  (message "📡 OSCSIGNAL -> SuperDirt is READY! 📡")
  (beep)
  (message "vivid-tidal-superdirt-startup-functions: %s"
    vivid-tidal-superdirt-startup-functions)
  (and vivid-tidal-superdirt-startup-functions
    (mapc #'funcall vivid-tidal-superdirt-startup-functions)))

(defun vivid-tidal-remove-emacs-osc-listener ()
  "Stop the OSC listener in Emacs on port 7777."
  (interactive)
  (and (get-process "OSCserver")
    (ignore-errors (delete-process "OSCserver")))
  (and vivid-tidal-osc-server
    (process-status vivid-tidal-osc-server)
    (delete-process vivid-tidal-osc-server)))

(defun vivid-tidal-start-emacs-osc-listener ()
  "Restart the OSC listener in Emacs on port 7777."
  (interactive)
  (vivid-tidal-remove-emacs-osc-listener)
  (message "👂 Emacs: Starting OSC listener on port 7777...")
  (setq vivid-tidal-osc-server
    (osc-make-server "127.0.0.1" 7777
      (lambda (path &rest args)
        (cond
          ((string= path "/dirt/ready")
            (run-hooks 'vivid-tidal-superdirt-boot-hook)))))))

(add-hook 'vivid-tidal-superdirt-boot-hook #'vivid-tidal-on-superdirt-ready-osc)

;;;###autoload
(defun vivid-tidal-start-superdirt ()
  "Start SuperDirt with extended memory options."
  (interactive)
  (vivid-tidal-start-emacs-osc-listener)
  (message "📡 Enviando configuración de SuperDirt a SCLang (port 7777)...")
  (sclang-eval-string
    "(
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

    // --- Vivid: Reload Listener (Robust) ---
    fork {
        thisProcess.openUDPPort(57120);
        0.1.wait;
        OSCdef('vividReload', { |msg|
           \"Vivid: -> RELOAD SIGNAL RECEIVED\".postln;
           fork {
              var path = \"/home/numa/.local/share/SuperCollider/synthdefs/\";
              s.sendMsg(\"/d_loadDir\", path);
              s.sync;
              SynthDescLib.global.read(path ++ \"*.scsyndef\");
              0.2.wait;
              ~dirt.loadSynthDefs;
              \"Vivid: -> Sync complete. Definitions updated.\".postln;
           }
        }, '/vivid/reload', recvPort: 57120).fix;
        \"Vivid: OSC Listener INSTALLED on port 57120\".postln;

        // --- Notify Emacs that SuperDirt and Vivid are ready (Port 7777) ---
        s.sync;
        NetAddr(\"127.0.0.1\", 7777).sendMsg(\"/dirt/ready\", \"SuperDirt is ready\");
        \"Vivid: SuperDirt is ready. Notifying Emacs on port 7777...\".postln;
    };
};
)"))

(add-hook 'sclang-library-startup-hook #'vivid-tidal-start-superdirt 95)

(provide 'vivid-tidal-superdirt-start)
;;; vivid-tidal-superdirt-start.el ends here

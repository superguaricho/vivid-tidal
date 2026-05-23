;;; vivid-tidal.el --- Start vivid-tidal in GHCi and SuperCollider -*- lexical-binding: t -*-
;;
;; Filename: vivid-tidal.el
;; Description: Start vivid-tidal in GHCi and SuperCollider
;; Author: Numa Tortolero
;; Maintainer: Numa Tortolero
;; Created: vie may  8 11:51:51 2026 (-0400)
;; Version: 0.1.0.6
;; Package-Requires: ((osc "0.4") (haskell-mode "17.5"))
;; URL: https://github.com/superguaricho/vivid-tidal-el
;; Keywords: haskell tidal supercollider live-coding
;; Compatibility: GNU Emacs 29.0.50
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;   This package provides functions to start vivid-tidal in GHCi and
;;  SuperCollider. It uses the haskell-live package to manage the GHCi
;;  session and the sclang package to manage the SuperCollider session.
;;  It also provides a test function to play some notes on startup.
;;
;;  This version synchronizes Haskell startup with SuperDirt being ready
;;  via OSC messages and uses native haskell-mode command queuing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(add-to-list 'load-path (file-name-directory
                          (or load-file-name buffer-file-name)))

(require 'haskell-live)
(require 'sclang)
(require 'vivid-tidal-layouts nil t)
(require 'vivid-tidal-superdirt-install nil t)
(require 'vivid-tidal-install nil t)
(require 'vivid-tidal-superdirt-start nil t)

;; (package-install-file (file-name-directory (or load-file-name buffer-file-name)))

(defvar vivid-tidal-session-name "vivid-tidal"
  "The name of the GHCi session for vivid-tidal.")

(defvar vivid-tidal-version "0.1.0.0"
  "The version of the vivid-tidal package.")

(defvar vivid-tidal-haskell-dir
  (expand-file-name "~/.local/share/haskell")
  "The directory where the haskell scripts for vivid-tidal are located.")

(defvar vivid-tidal-path
  (expand-file-name (format "%s/%s"
                      vivid-tidal-haskell-dir
                      vivid-tidal-session-name))
  "The path to the vivid-tidal package.")

(defvar vivid-tidal-repl-buffer
  (format "*%s*" vivid-tidal-session-name)
  "The name of the buffer for vivid-tidal session.")

(defvar vivid-tidal-ghci-script
  (expand-file-name
    (format "%s%s.ghci" vivid-tidal-session-name "-emacs")
    vivid-tidal-path)
  "The name of the GHCi script to load vivid-tidal.")

(defvar vivid-tidal-command
  (let ((script vivid-tidal-ghci-script))
    (if (file-exists-p script)
      (format ":script %s" script)
      nil))
  "Command to load tidalcycles in ghci.")

(defvar vivid-tidal--target-buffer-name nil
  "Internal variable to store the name of the buffer to be linked.")

(defconst vivid-tidal-test-string
  ":{
import Vivid as V

t = sd (0 ::I \"note\") $ do
      e <- line (start_ 0.2, end_ 0, duration_ 0.8, doneAction_ 2)
      w <- sinOsc (freq_ $ midiCPS (V::V \"note\"))
      s <- e ~* w
      out 0 [s, s]

playNote s f d = synth s (f :: I \"note\") >> V.wait d

do playNote t 60 0.25 >> playNote t 67 0.25 >> playNote t 72 2.0
:}
")

(defvar vivid-tidal-test t
  "Whether to play test notes on startup.")

(declare-function tidal-layout-3 "tidal-layouts" () t)

(defun vivid-tidal-send-command (command)
  "Send COMMAND to the vivid-tidal session."
  (let* ((session (haskell-live-get-session-by-name
                    vivid-tidal-session-name))
          (proc (and session (haskell-session-process session))))
    (and proc (process-live-p (haskell-process-process proc))
      (haskell-process-send-string proc command))))

(defun vivid-tidal-boot ()
  "Send the `:boot' command to the vivid-tidal session."
  (interactive)
  (vivid-tidal-send-command ":boot"))

(defun vivid-tidal-test ()
  "Send the `vivid-tidal-test-string' command to the vivid-tidal session."
  (interactive)
  (vivid-tidal-send-command vivid-tidal-test-string))

(setq vivid-tidal-test nil)

(defun vivid-tidal-startup ()
  "This function runs when GHCi is starting.
Synchronized via GHCi script prompt \\4."
  (interactive)
  (let ((proc (haskell-process)))
    (if proc
      (progn
        (haskell-process-queue-command
          proc
          (make-haskell-command
            :state proc
            :go (lambda (p)
                  (haskell-process-send-string p
                    vivid-tidal-command)
                  (message
                    "⏳ Initializing vivid-tidal with vivid-tidal.ghci..."))))
        (haskell-process-queue-command
          proc
          (make-haskell-command
            :state proc
            :go (lambda (p)
                  (haskell-process-send-string p ":boot")
                  (message "🚀 Booting Tidal/Vivid environment..."))
            :complete (lambda (p _)
                        (when (fboundp 'vivid-tidal-layouts-layout-3) (vivid-tidal-layouts-layout-3))
                        (and vivid-tidal-test
                          (haskell-process-send-string p
                            vivid-tidal-test-string))
                        (message "✨ vivid-tidal ready and synchronized!")))))
      (message "⚠️ Haskell process has not been found."))))

(defvar vivid-tidal-superdirt-startup-functions nil
  "List of functions to run when SuperDirt starts up.")

;;;###autoload
(defun vivid-tidal-run ()
  "Run interactive vivid-tidal process."
  (interactive)
  (message "🚀 Triggering vivid-tidal Haskell startup...")
  (remove-hook 'sclang-library-startup-hook 'vivid-tidal-run)
  (let ((haskell-buffer (haskell-live-get-haskell-buffer)))
    (if (not (get-buffer vivid-tidal-repl-buffer))
      (let* ((session (haskell-live-get-session-by-name vivid-tidal-session-name))
              (proc (and session (haskell-session-process session))))
        (and proc (process-live-p (haskell-process-process proc))
          (kill-process (haskell-process-process proc)))
        (setq haskell-process-type 'cabal-repl)
        (message "🔄 starting %s session..." vivid-tidal-session-name)
        (if haskell-buffer
          (with-current-buffer haskell-buffer
            (haskell-live-process-start
              vivid-tidal-session-name
              vivid-tidal-path
              nil
              'vivid-tidal-startup))
          (haskell-live-process-start
            vivid-tidal-session-name
            vivid-tidal-path
            nil
            'vivid-tidal-startup)))
      (haskell-live-add-to-session vivid-tidal-session-name))))

(declare-function vivid-tidal-start-superdirt "vivid-tidal-superdirt-start" () t)

;;;###autoload
(defun vivid-tidal-sclang ()
  "Start sclang and wait for SuperDirt to be ready before starting vivid-tidal."
  (interactive)
  (add-to-list 'vivid-tidal-superdirt-startup-functions #'vivid-tidal-run)
  (add-hook 'sclang-library-startup-hook #'hsc3-tidal-start-superdirt 95)
  (let ((proc (get-process sclang-process)))
    (if (and proc (process-live-p proc))
      (tidal-start-superdirt)
      (sclang-start))))

(keymap-set haskell-mode-map "C-c :" #'vivid-tidal-sclang)

;;;###autoload
(defalias 'vivid-tidal 'vivid-tidal-sclang)

;;;###autoload
(defun vivid-tidal-tidal ()
  "Restart the vivid-tidal session."
  (interactive)
  (haskell-process-show-repl-response vivid-tidal-command))

;;;

(defun vivid-tidal-kill-sc-buffers ()
  "Kill the supercollider buffers."
  (interactive)
  (let ((wsbuf (get-buffer "*SClang:Workspace*"))
         (postbuf (get-buffer sclang-post-buffer)))
    (and wsbuf (kill-buffer wsbuf))
    (and postbuf (kill-buffer postbuf))))

(defun vivid-tidal-kill-sclang ()
  "Kill the supercollider system."
  (interactive)
  (sclang-kill)
  (vivid-tidal-kill-sc-buffers))

;;;###autoload
(defun vivid-tidal-kill ()
  "kill the ghci vivid-tidal session."
  (interactive)
  (let ((main-buffer (current-buffer)))
    (and (or (get-process sclang-process)
           (get-buffer sclang-post-buffer))
      (kill-sclang))
    (and (get-buffer vivid-tidal-repl-buffer)
      (kill-buffer vivid-tidal-repl-buffer))
    (select-window (sclang-ext-get-main-window))
    (delete-other-windows)
    (switch-to-buffer main-buffer)))
(keymap-set haskell-mode-map "C-c ;" #'vivid-tidal-kill)

;;;###autoload
(defalias 'kill-vivid-tidal 'vivid-tidal-kill)

(provide 'vivid-tidal)
;;; vivid-tidal.el ends here

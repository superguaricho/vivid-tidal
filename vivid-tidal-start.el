;;; vivid-tidal-start.el --- Start vivid-tidal in GHCi and SuperCollider
;;
;; Filename: vivid-tidal-start.el
;; Description: Start vivid-tidal in GHCi and SuperCollider
;; Author: Numa Tortolero
;; Maintainer: Numa Tortolero
;; Created: vie may  8 11:51:51 2026 (-0400)
;; Version: 0.1.0.2
;; URL: https://github.com/superguaricho/vivid-tidal
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
(require 'sclang-ext-layout-for-3 nil t)
(require 'vivid-tidal-superdirt-install nil t)
(require 'vivid-tidal-install nil t)
(require 'vivid-tidal-superdirt-start nil t)

(defvar vivid-tidal-start-session-name "vivid-tidal"
  "The name of the GHCi session for vivid-tidal.")

(defvar vivid-tidal-start-version "0.1.0.0"
  "The version of the vivid-tidal package.")

(defvar vivid-tidal-start-haskell-dir
  (expand-file-name "~/.local/share/haskell")
  "The directory where the haskell scripts for vivid-tidal are located.")

(defvar vivid-tidal-start-path
  (expand-file-name (format "%s/%s"
                      vivid-tidal-start-haskell-dir
                      vivid-tidal-start-session-name))
  "The path to the vivid-tidal package.")

(defvar vivid-tidal-start-repl-buffer
  (format "*%s*" vivid-tidal-start-session-name)
  "The name of the buffer for vivid-tidal session.")

(defvar vivid-tidal-start-ghci-script
  (expand-file-name
    (format "%s%s.ghci" vivid-tidal-start-session-name "-emacs")
    vivid-tidal-start-path)
  "The name of the GHCi script to load vivid-tidal.")

(defvar vivid-tidal-start-command
  (let ((script vivid-tidal-start-ghci-script))
    (if (file-exists-p script)
      (format ":script %s" script)
      nil))
  "Command to load tidalcycles in ghci.")

(defvar vivid-tidal-start--target-buffer-name nil
  "Internal variable to store the name of the buffer to be linked.")

(defconst vivid-tidal-start-test-string
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

(defvar vivid-tidal-start-test t
  "Whether to play test notes on startup.")

(declare-function tidal-layout-3 "tidal-layouts" () t)

(defun vivid-tidal-start-send-command (command)
  "Send COMMAND to the vivid-tidal session."
  (let* ((session (haskell-live-get-session-by-name
                    vivid-tidal-start-session-name))
          (proc (and session (haskell-session-process session))))
    (and proc (process-live-p (haskell-process-process proc))
      (haskell-process-send-string proc command))))

(defun vivid-tidal-start-boot ()
  "Send the `:boot' command to the vivid-tidal session."
  (interactive)
  (vivid-tidal-start-send-command ":boot"))

(defun vivid-tidal-start-test ()
  "Send the `vivid-tidal-start-test-string' command to the vivid-tidal session."
  (interactive)
  (vivid-tidal-start-send-command vivid-tidal-start-test-string))

(setq vivid-tidal-start-test nil)

(defun vivid-tidal-start-startup ()
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
                    vivid-tidal-start-command)
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
                        (when (fboundp 'tidal-layout-3) (tidal-layout-3))
                        (and vivid-tidal-start-test
                          (haskell-process-send-string p
                            vivid-tidal-start-test-string))
                        (message "✨ vivid-tidal ready and synchronized!")))))
      (message "⚠️ Haskell process has not been found."))))

(defvar vivid-tidal-superdirt-startup-functions nil
  "List of functions to run when SuperDirt starts up.")

;;;###autoload
(defun vivid-tidal-start-run ()
  "Run interactive vivid-tidal process."
  (interactive)
  (message "🚀 Triggering vivid-tidal Haskell startup...")
  (remove-hook 'sclang-library-startup-hook 'vivid-tidal-start-run)
  (let ((haskell-buffer (haskell-live-get-haskell-buffer)))
    (if (not (get-buffer vivid-tidal-start-repl-buffer))
      (let* ((session (haskell-live-get-session-by-name vivid-tidal-start-session-name))
              (proc (and session (haskell-session-process session))))
        (and proc (process-live-p (haskell-process-process proc))
          (kill-process (haskell-process-process proc)))
        (setq haskell-process-type 'cabal-repl)
        (message "🔄 starting %s session..." vivid-tidal-start-session-name)
        (if haskell-buffer
          (with-current-buffer haskell-buffer
            (haskell-live-process-start
              vivid-tidal-start-session-name
              vivid-tidal-start-path
              nil
              'vivid-tidal-start-startup))
          (haskell-live-process-start
            vivid-tidal-start-session-name
            vivid-tidal-start-path
            nil
            'vivid-tidal-start-startup)))
      (haskell-live-add-to-session vivid-tidal-start-session-name))))

;;;###autoload
(defun vivid-tidal-start-sclang ()
  "Start sclang and wait for SuperDirt to be ready before starting vivid-tidal."
  (interactive)
  (require 'tidal-superdirt-start)
  (add-to-list 'vivid-tidal-superdirt-startup-functions #'vivid-tidal-start-run)
  (let ((proc (get-process sclang-process)))
    (if (and proc (process-live-p proc))
      (tidal-start-superdirt)
      (sclang-start))))

(keymap-set haskell-mode-map "C-c >" #'vivid-tidal-start-sclang)

(defalias 'turpial 'vivid-tidal-start-sclang)

;;;###autoload
(defalias 'vivid-tidal-start 'vivid-tidal-start-sclang)

;;;###autoload
(defun vivid-tidal-start-tidal ()
  "Restart the vivid-tidal session."
  (interactive)
  (haskell-process-show-repl-response vivid-tidal-start-command))

;;;

(defun vivid-tidal-start-kill-sc-buffers ()
  "Kill the supercollider buffers."
  (interactive)
  (let ((wsbuf (get-buffer "*SClang:Workspace*"))
         (postbuf (get-buffer sclang-post-buffer)))
    (and wsbuf (kill-buffer wsbuf))
    (and postbuf (kill-buffer postbuf))))

(defun vivid-tidal-start-kill-sclang ()
  "Kill the supercollider system."
  (interactive)
  (sclang-kill)
  (vivid-tidal-start-kill-sc-buffers))

;;;###autoload
(defun vivid-tidal-start-kill ()
  "kill the ghci vivid-tidal session."
  (interactive)
  (let ((main-buffer (current-buffer)))
    (and (or (get-process sclang-process)
           (get-buffer sclang-post-buffer))
      (kill-sclang))
    (and (get-buffer vivid-tidal-start-repl-buffer)
      (kill-buffer vivid-tidal-start-repl-buffer))
    (select-window (sclang-ext-get-main-window))
    (delete-other-windows)
    (switch-to-buffer main-buffer)))
(keymap-set haskell-mode-map "C-c z" #'vivid-tidal-start-kill)

;;;###autoload
(defalias 'kill-vivid-tidal 'vivid-tidal-start-kill)

(provide 'vivid-tidal-start)
;;; vivid-tidal-start.el ends here

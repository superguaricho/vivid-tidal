;;; vivid-tidal-install.el --- Installation script for vivid-tidal -*- lexical-binding: t -*-
;;
;; Filename: vivid-tidal-install.el
;; Description: Emacs Lisp script to automate the installation of vivid-tidal.
;; Author: Numa Tortolero
;; Created: mié may 13 20:20:23 2026 (-0400)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  This Emacs Lisp script automates the installation of vivid-tidal,
;; a Haskell package for live coding music with SuperCollider. It clones
;; the Vivid-Tidal repository from GitHub, builds it using Cabal, and
;; sets up executable scripts for easy access. The installation
;; process is designed to be user-friendly, providing real-time feedback
;; in an Emacs buffer.
;; Users can also stop the installation process if needed and clean up
;; installation files if they wish to start fresh.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar vivid-tidal-install-haskell-local-dir
  (expand-file-name "~/.local/share/haskell")
  "Base directory for local Haskell installations.
vivid-tidal will be installed in a subdirectory here.")

(defvar vivid-tidal-install-pack-name "vivid-tidal"
  "Name of the Vivid-Tidal package for installation and script generation.")

(defvar vivid-tidal-install-repo
  (format "https://github.com/superguaricho/%s" vivid-tidal-install-pack-name)
  "Name of the Vivid-Tidal package for installation and script generation.")

(defvar vivid-tidal-install-buffer
  (format "*%s installation*" vivid-tidal-install-pack-name))

(defvar vivid-tidal-install-version ""
  "Version of vivid-tidal to install.
Update this to install a different version.")

(defvar vivid-tidal-install-dir
  (expand-file-name
    vivid-tidal-install-pack-name vivid-tidal-install-haskell-local-dir)
  "Directory where TidalCycles source will be cloned and built.")

(defvar vivid-tidal-install-user-bin-dir (expand-file-name "~/.local/bin")
  "Directory where the Tidal executable script will be placed.")

(defvar vivid-tidal-install-bash-script
  (expand-file-name vivid-tidal-install-pack-name vivid-tidal-install-user-bin-dir)
  "Path to the Vivid-Tidal executable script.")

(defvar vivid-tidal-install-ghci-script
  (expand-file-name (format "%s-emacs.ghci" vivid-tidal-install-pack-name)
    vivid-tidal-install-dir)
  "Path to the Vivid-Tidal executable script.")

(defvar vivid-tidal-install-ghci-script-file
  (expand-file-name "loadme.ghci" vivid-tidal-install-dir)
  "Path to the Vivid-Tidal executable script.")

(defvar vivid-tidal-install--spinner-state 0
  "Internal state for the installation spinner animation.")

(defconst vivid-tidal-install--spinner-chars ["|" "/" "-" "\\"]
  "Characters used for the spinner animation in the mode-line.")

(defun vivid-tidal-install--update-spinner (proc)
  "Update the spinner in the mode-line for PROC."
  (when (process-live-p proc)
    (with-current-buffer (process-buffer proc)
      (setq vivid-tidal-install--spinner-state
        (% (1+ vivid-tidal-install--spinner-state) 4))
      (setq mode-line-process
        (format " [%s] %s"
          (aref vivid-tidal-install--spinner-chars
            vivid-tidal-install--spinner-state)
          (process-status proc)))
      (force-mode-line-update))))

(defun vivid-tidal-install-process-filter (proc string)
  "Process PROC filter for process vivid-tidal installation output STRING.
Updates the buffer with output from proc and string, and updates the spinner."
  (vivid-tidal-install--update-spinner proc)
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

;;;###autoload
(defun vivid-tidal-install-stop ()
  "Stop any active Tidal installation process."
  (interactive)
  (let ((proc (get-buffer-process vivid-tidal-install-buffer)))
    (if (and proc (process-live-p proc))
      (progn
        (set-process-sentinel proc nil) ;; Remove sentinel to avoid triggering build steps
        (kill-process proc)
        (message "🛑 Vivid-Tidal installation process stopped."))
      (message "No active installation process found."))))

(defun vivid-tidal-install-repo ()
  "Install the Vivid-Tidal package from git using cabal.
Returns the process object."
  (interactive)
  (or (file-directory-p vivid-tidal-install-haskell-local-dir)
    (make-directory vivid-tidal-install-haskell-local-dir t))
  (let ((coding-system-for-read 'utf-8-unix)
         (process-connection-type t))
    (with-current-buffer (get-buffer-create vivid-tidal-install-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq-local window-point-insertion-type t)
      (setq-local scroll-conservatively 101)
      (add-hook 'kill-buffer-hook
        (lambda ()
          (let ((p (get-buffer-process (current-buffer))))
            (when (and p (process-live-p p)) (kill-process p))))
        nil t)
      (cd vivid-tidal-install-haskell-local-dir)
      (let ((proc (start-process
                    "vivid-tidal-install"
                    vivid-tidal-install-buffer
                    "git" "clone" vivid-tidal-install-repo)))
        (when proc
          (set-process-filter proc 'vivid-tidal-install-process-filter)
          (display-buffer vivid-tidal-install-buffer
            '((display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.1)
               (side . bottom))))
        proc))))

(defun vivid-tidal-install-build-repo ()
  "Compiles the Vivid-Tidal package during the installation.
Returns the process object."
  (interactive)
  (if (and (executable-find "cabal")
        (file-directory-p vivid-tidal-install-dir))
    (let ((coding-system-for-read 'utf-8-unix)
           (default-directory vivid-tidal-install-dir)
           (process-connection-type t))
      (with-current-buffer (get-buffer-create vivid-tidal-install-buffer)
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert "\n--- Starting Cabal Build ---\n"))
        (cd vivid-tidal-install-dir)
        (let ((proc (start-process-shell-command
                      "vivid-tidal-build"
                      vivid-tidal-install-buffer
                      "cabal build")))
          (set-process-filter proc 'vivid-tidal-install-process-filter)
          (display-buffer vivid-tidal-install-buffer
            '((display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.4)
               (side . bottom)))
          proc)))
    (message "⚠️ Cabal not found or Vivid-Tidal directory missing.")
    nil))

;;;###autoload
(defun vivid-tidal-install-build ()
  "Install and build Vivid-Tidal sequentially using process sentinels."
  (interactive)
  (let ((clone-proc (vivid-tidal-install-repo)))
    (set-process-sentinel
      clone-proc
      (lambda (p event)
        (cond
          ((string-match-p "finished" event)
            (message "✅ Repo cloned. Starting build...")
            (let ((build-proc (vivid-tidal-install-build-repo)))
              (when build-proc
                (set-process-sentinel
                  build-proc
                  (lambda (p2 event2)
                    (if (string-match-p "finished" event2)
                      (progn
                        (message "✅ Vivid-Tidal built successfully!")
                        (vivid-tidal-install-scripts))
                      (message "❌ Build failed: %s" event2)))))))
          (t (message "❌ Clone failed: %s" event)))))))

;;;###autoload
(defun vivid-tidal-install-bash-script ()
  "Install the vivid-tidal executable bash script in `vivid-tidal-install-user-bin-dir'."
  (interactive)
  (unless (file-directory-p vivid-tidal-install-user-bin-dir)
    (make-directory vivid-tidal-install-user-bin-dir t))
  (if (file-directory-p vivid-tidal-install-user-bin-dir)
    (progn
      (with-temp-file vivid-tidal-install-bash-script
        (insert vivid-tidal-install-bash-script-string))
      (set-file-modes vivid-tidal-install-bash-script #o755)
      (message
        "✅ Vivid-Tidal bash script installed at %s"
        vivid-tidal-install-bash-script)))
  (message "❌ Error: Could not create directory %s"
    vivid-tidal-install-user-bin-dir))

;;;###autoload
(defun vivid-tidal-install-ghci-scripts ()
  "Install the .ghci initialization files in the haskell local dir."
  (interactive)
  (when (file-directory-p vivid-tidal-install-dir)
    (with-temp-file vivid-tidal-install-ghci-script
      (insert vivid-tidal-install-ghci-script-string))
    (with-temp-file vivid-tidal-install-ghci-script-file
      (insert vivid-tidal-install-ghci-loadme-string))
    (message "✅ vivid-tidal GHCi scripts installed at %s"
      vivid-tidal-install-dir)))

;;;###autoload
(defun vivid-tidal-install-scripts ()
  "Install both bash and GHCi scripts."
  (interactive)
  (vivid-tidal-install-bash-script)
  (vivid-tidal-install-ghci-scripts))

;;;###autoload
(defun vivid-tidal-install ()
  "Perform the complete vivid-tidal installation sequentially (Clone -> Build -> Scripts)."
  (interactive)
  (message "🚀 Starting vivid-tidal installation...")
  (let ((clone-proc (vivid-tidal-install-repo)))
    (if clone-proc
      (set-process-sentinel
        clone-proc
        (lambda (p event)
          (cond
            ((string-match-p "finished" event)
              (message "✅ Repo cloned. Starting build...")
              (let ((build-proc (vivid-tidal-install-build-repo)))
                (if build-proc
                  (set-process-sentinel
                    build-proc
                    (lambda (p2 event2)
                      (if (string-match-p "finished" event2)
                        (progn
                          (message "✅ vivid-tidal built successfully!")
                          ;; Los scripts se instalan SOLO después del build con éxito
                          (vivid-tidal-install-scripts))
                        (message "❌ Build failed: %s" event2))))
                  (message "❌ Could not start build process."))))
            ((string-match-p "\\(aborted\\|exited\\|failed\\)" event)
              (message "❌ Clone failed: %s" event)))))
      (message "❌ Could not start clone process."))))
;;;;

(defun vivid-tidal-install-quit-buffer ()
  "Close the vivid-tidal installation buffer and delete its window."
  (interactive)
  (let ((buffer (get-buffer vivid-tidal-install-buffer)))
    (and buffer
      (let ((window (get-buffer-window buffer)))
        (and window (quit-window t window))
        (and buffer (buffer-live-p buffer) (kill-buffer buffer))))))
(defalias 'quit-install 'vivid-tidal-install-quit-buffer)

(defun vivid-tidal-install-clean-installation ()
  "Remove Vivid-Tidal installation files and directories."
  (interactive)
  (when (file-directory-p vivid-tidal-install-dir)
    (delete-directory vivid-tidal-install-dir t))
  (when (file-exists-p vivid-tidal-install-bash-script)
    (delete-file vivid-tidal-install-bash-script))
  (message "🧹 Vivid-Tidal installation cleaned."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vivid-tidal-install-url
  "https://github.com/tidalcycles/Tidal/archive/refs/heads/main.tar.gz"
  "URL to download the TidalCycles source code.")

(defconst vivid-tidal-install-bash-script-string
  (format "#!/bin/bash
PACK=%s
PACKPATH=%s/$PACK
cd $PACKPATH
cabal repl --repl-options=\"-ghci-script ${PACK}.ghci\" --repl-options=-Wno-missing-home-modules"
    vivid-tidal-install-pack-name
    vivid-tidal-install-haskell-local-dir)
  "Content of the Vivid-Tidal executable bash script.")

(defconst vivid-tidal-install-ghci-script-string
  (format "
:def! boot \\_ -> return \":script loadme.ghci\"

:{
let line  = cyan ++ replicate 36 '-' ++ reset
in do
  putStrLn \"\"
  putStrLn line
  putStrLn $ \"| \" \"Type \" \":boot\" ++ \" to start %s.\" ++ \" |\"
  putStrLn line
  putStrLn \"\"
:}
" vivid-tidal-install-pack-name))

(defconst vivid-tidal-install-ghci-loadme-string
  (format "
:cd %s
:set +m

:set prompt \"\"
:set prompt-cont \"\"

:set -fno-warn-orphans -Wno-type-defaults -XMultiParamTypeClasses -XOverloadedStrings
:set prompt \"\"

:set -XOverloadedStrings

import   Vivid as V

:{
t = sd (0 :: I \"note\") $ do
    e <- line (start_ 0.2, end_ 0, duration_ 0.8, doneAction_ 2)
    w <- sinOsc (freq_ $ midiCPS (V :: V \"note\"))
    s <- e ~* w
    out 0 [s, s]

playNote :: SynthDef '[\"note\"] -> I \"note\" -> Float -> IO ()
playNote s f d = do synth s (f :: I \"note\") >> V.wait d
:}

-- do playNote t 60 0.25 >> playNote t 67 0.25 >> playNote t 72 2.0

data Note a = Note Int Float

pitch' (Note p _) = p

dur' (Note _ d) = d

:{
playNotes :: SynthDef '[\"note\"] -> [Note a] -> IO ()
playNotes _ [] = return ()
playNotes s (n : ns) = do
    playNote s (fromIntegral (pitch' n)) (dur' n)
    playNotes s ns
:}

notes = [Note 60 0.25, Note 67 0.25, Note 72 2.0]

vividSplash = unlines $ [
                          \"____   ____.__      .__    .___\",
                          \"\\\\   \\\\ /   /|__|__  _|__| __| _/\",
                          \" \\\\   y   / |  \\\\  \\\\/ /  |/ __ | \",
                          \"  \\\\     /  |  |\\\\   /|  / /_/ | \",
                          \"   \\\\___/   |__| \\\\_/ |__\\\\____ | \",
                          \" \",
                          \"Sound synthesis with SuperCollider.\",
                          \"(c) 2024 Vivid, Tom Murphy.\",
                          \" \"]

main = do playNotes t notes >> putStrLn vividSplash

main

:l src/Vivid/Tidal.hs
:script BootTidal.hs

:set prompt \"tidal> \"
:set prompt \"\\4\"
"
    vivid-tidal-install-dir)
  "template for the vivid-tidal.ghci file with emacs support.")

(provide 'vivid-tidal-install)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vivid-tidal-install.el ends here

;;; project-persist-drawer-adaptor-sr-speebar.el --- Sr Speedbar Adaptor for project-persist-drawer

;; Copyright (C) 2015 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/project-persist-drawer-adaptor-sr-speedbar
;; Version: 0.0.1
;; Created: 2015-04-18
;; Keywords: projects, drawer

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; #project-persist-drawer-adaptor-sr-speedbar
;; 
;; This is an adaptor allowing
;; [project-persist-drawer](https://github.com/rdallasgray/project-persist-drawer)
;; to use [Sr Speedbar](https://github.com/emacsmirror/sr-speedbar) as
;; its drawer implementation.
;;
;;; Code:

(require 'sr-speedbar)
(require 'project-persist-drawer)

;;; Interface

(eval-after-load 'project-persist-drawer
  '(progn
     (defun project-persist-drawer--get-window ()
       (when (and (fboundp 'sr-speedbar-exist-p)
                  (sr-speedbar-exist-p))
         sr-speedbar-window))

     (defun project-persist-drawer--open (dir)
       (let ((window-state (window-state-get)))
         (delete-other-windows (frame-first-window))
         (sr-speedbar-open)
         (speedbar-update-contents)
         (window-state-put window-state (window-next-sibling sr-speedbar-window))
         (ppdss/pin dir)
         (ppdss/rededicate)))

     (defun project-persist-drawer--close ()
       (ppdss/undedicate)
       (ppdss/unpin)
       (sr-speedbar-close))

;;; Internal

     (defun ppdss/undedicate ()
       (ppdss/try-set-window-dedication nil))

     (defun ppdss/rededicate ()
       (ppdss/try-set-window-dedication t))

     (defun ppdss/try-set-window-dedication (p)
       (let ((window (project-persist-drawer--get-window)))
         (when window
           (set-window-dedicated-p window p))))

     (defun ppdss/pin (dir)
       "Prevent the speedbar from changing the displayed root directory."
       (setq ppdss/pinned-directory dir)
       (mapc (lambda (ls) (apply 'ad-enable-advice ls)) ppdss/pin-advice)
       (ppdss/pin-advice-activate))

     (defun ppdss/unpin ()
       (mapc (lambda (ls) (apply 'ad-disable-advice ls)) ppdss/pin-advice)
       (ppdss/pin-advice-activate))

     (defun ppdss/pin-advice-activate ()
       "Activate the advice applied to speedbar functions in order to pin it to a directory."
       (mapc 'ad-activate (mapcar 'car ppdss/pin-advice)))

     (defun ppdss/setup-pinning ()
       (defadvice speedbar-update-directory-contents
           (around ppdss/pin-directory activate disable)
         "Pin the speedbar to the directory set in ppdss/pinned-directory."
         (let ((default-directory ppdss/pinned-directory))
           ad-do-it))
       (defadvice speedbar-dir-follow
           (around ppdss/prevent-follow activate disable)
         "Prevent speedbar changing directory on button clicks."
         (speedbar-toggle-line-expansion))
       (defadvice speedbar-directory-buttons-follow
           (around ppdss/prevent-root-follow activate disable)
         "Prevent speedbar changing root directory on button clicks.")
       (defvar ppdss/pin-advice
         '((speedbar-update-directory-contents around ppdss/pin-directory)
           (speedbar-dir-follow around ppdss/prevent-follow)
           (speedbar-directory-buttons-follow around ppdss/prevent-root-follow))))

     (defun ppdss/load-settings ()
       (setq speedbar-hide-button-brackets-flag t
             speedbar-show-unknown-files t
             speedbar-smart-directory-expand-flag t
             speedbar-directory-button-trim-method 'trim
             speedbar-use-images nil
             speedbar-indentation-width 2
             speedbar-use-imenu-flag t
             sr-speedbar-width 40
             sr-speedbar-width-x 40
             sr-speedbar-auto-refresh nil
             sr-speedbar-skip-other-window-p t
             sr-speedbar-right-side nil))

     (defvar ppdss/refresh-hooks '(after-save-hook))
     (defvar ppdss/refresh-hooks-added nil)

     (defun ppdss/add-refresh-hooks ()
       (when (not ppdss/refresh-hooks-added)
         (lambda ()
           (mapc (lambda (hook)
                   (add-hook hook 'speedbar-refresh))
                 ppdss/refresh-hooks)
           (setq ppdss/refresh-hooks-added t))))

     (defun ppdss/setup-speedbar ()
       (add-hook 'speedbar-mode-hook
                 '(lambda ()
                    (hl-line-mode 1)
                    (visual-line-mode -1)
                    (setq automatic-hscrolling nil)
                    (let ((speedbar-display-table (make-display-table)))
                      (set-display-table-slot speedbar-display-table 0 8230)
                      (setq buffer-display-table speedbar-display-table)))))

     (defun ppdss/setup-keymap ()
       (add-hook 'speedbar-reconfigure-keymaps-hook
                 '(lambda ()
                    (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
                    (define-key speedbar-mode-map [left] 'speedbar-contract-line))))

     (defvar ppdss/target-window
       (if (not (eq (selected-window) sr-speedbar-window))
           (selected-window)
         (other-window 1)))

     (defun ppdss/select-target-window ()
       (message "selecting target window")
       (select-window ppdss/target-window))

     (defun ppdss/setup-target-window ()
       (defadvice select-window (after remember-selected-window activate)
         (unless (or (eq (selected-window) sr-speedbar-window)
                     (not (window-live-p (selected-window))))
           (setq ppdss/target-window (selected-window)))))

     (eval-after-load 'sr-speedbar
       '(progn
          (ppdss/load-settings)
          (ppdss/add-refresh-hooks)
          (ppdss/setup-speedbar)
          (ppdss/setup-keymap)
          (ppdss/setup-target-window)
          (ppdss/setup-pinning)

          ;; Overrides
          (defun sr-speedbar-before-visiting-file-hook ()
            "Function that hooks `speedbar-before-visiting-file-hook'."
            (ppdss/select-target-window))

          (defun sr-speedbar-before-visiting-tag-hook ()
            "Function that hooks `speedbar-before-visiting-tag-hook'."
            (ppdss/select-target-window))

          (defun sr-speedbar-visiting-file-hook ()
            "Function that hooks `speedbar-visiting-file-hook'."
            (ppdss/select-target-window))

          (defun sr-speedbar-visiting-tag-hook ()
            "Function that hooks `speedbar-visiting-tag-hook'."
            (ppdss/select-target-window))))))

(provide 'project-persist-drawer-adaptor-sr-speedbar)
;;; project-persist-drawer-adaptor-sr-speedbar.el ends here

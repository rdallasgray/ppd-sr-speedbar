(require 'sr-speedbar)

(defun project-persist-drawer-get-window ()
  (when (boundp 'sr-speedbar-window) sr-speedbar-window))

(defun project-persist-drawer-open (dir)
  (sr-speedbar-open)
  (speedbar-update-contents)
  (ppd/speedbar-pin dir))

(defun project-persist-drawer-before-open (dir)
  (ppd/speedbar-undedicate)
  (ppd/speedbar-unpin))

(defun project-persist-drawer-after-open (dir)
    (ppd/speedbar-pin dir)
    (ppd/speedbar-rededicate)))

;;; Internal

(defun ppd/speedbar-undedicate ()
  (ppd/try-set-window-dedication nil))

(defun ppd/speedbar-rededicate ()
  (ppd/try-set-window-dedication t))

(defun ppd/try-set-window-dedication (p)
  (let ((window (project-persist-drawer-get-window)))
    (when window
      (set-window-dedicated-p window p))))

(defun ppd/speedbar-pin (dir)
  "Prevent the speedbar from changing the displayed root directory."
  (setq ppd/speedbar-pinned-directory dir)
  (mapc (lambda (ls) (apply 'ad-enable-advice ls)) ppd/speedbar-pin-advice)
  (ppd/speedbar-pin-advice-activate))

(defun ppd/speedbar-unpin ()
  ((mapc (lambda (ls) (apply 'ad-disable-advice ls)) ppd/speedbar-pin-advice)
   (ppd/speedbar-pin-advice-activate)))

(defun ppd/speedbar-pin-advice-activate ()
  "Activate the advice applied to speedbar functions in order to pin it to a directory."
  (mapc 'ad-activate (mapcar 'car ppd/speedbar-pin-advice)))

(defvar ppd/speedbar-pin-advice
  '((speedbar-update-directory-contents around ppd/speedbar-pin-directory)
    (speedbar-dir-follow around ppd/speedbar-prevent-follow)
    (speedbar-directory-buttons-follow around ppd/speedbar-prevent-root-follow)))

(defadvice speedbar-update-directory-contents
    (around ppd/speedbar-pin-directory activate disable)
  "Pin the speedbar to the directory set in ppd/speedbar-pinned-directory."
  (let ((default-directory ppd/speedbar-pinned-directory))
    ad-do-it))

(defadvice speedbar-dir-follow
  (around ppd/speedbar-prevent-follow activate disable)
  "Prevent speedbar changing directory on button clicks."
  (speedbar-toggle-line-expansion))

(defadvice speedbar-directory-buttons-follow
  (around ppd/speedbar-prevent-root-follow activate disable)
  "Prevent speedbar changing root directory on button clicks.")

(provide 'project-persist-drawer-adaptor-sr-speedbar)

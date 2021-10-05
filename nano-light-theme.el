
(defun nano-new-frame (&optional mode)
  (interactive)
  (let ((mode (or mode (frame-parameter nil 'background-mode)))
        (background-mode frame-background-mode)
        (selected-frame (selected-frame))
        (new-frame nil))

    ;; Set mode
    (setq frame-background-mode mode)
    (setq new-frame (make-frame-command))
    (select-frame new-frame)

    ;; This forces recomputation of faces on the new frame
    (frame-set-background-mode (selected-frame))
           
    (when (eq mode 'light)
      (set-foreground-color nano-light-foreground)
      (set-background-color nano-light-background))

    (when (eq mode 'dark)
      (set-foreground-color nano-dark-foreground)
      (set-background-color nano-dark-background))

    ;; Restore background mode
    (setq frame-background-mode background-mode)
    (frame-set-background-mode selected-frame)
    
    new-frame))


(nano-new-frame 'light)
(nano-new-frame 'dark)
(nano-new-frame)


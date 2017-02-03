(defun symbol-at-point()
  (thing-at-point 'symbol))

(defun current-directory()
  (file-name-directory buffer-file-name))

(defun highlight-symbol (sym)
  (highlight-regexp sym 'hi-green-b)
  )

(defun get-ag-result-buffer(sym directory)
  (concat "*ag search text:" sym
          " dir:" directory "*")
  )

(defun find-in-directory()
  (interactive)
  (let (sym directory)
    (setq sym (symbol-at-point))
    (setq directory (current-directory))

    ;; search using ag
    (ag/search sym directory)

    ;; switch to result buffer
    (switch-to-buffer-other-window
     (get-ag-result-buffer sym directory))

    ;; highllight the symbol in the result buffer
    (highlight-symbol sym)
    )
  )

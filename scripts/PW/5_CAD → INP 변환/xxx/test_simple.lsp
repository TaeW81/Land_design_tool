;;; test_simple.lsp
;;; Simple test file

(defun c:TestSimple ()
  (princ "\nHello World!")
  (princ))

(defun c:TestAdd ()
  (setq a 5)
  (setq b 3)
  (setq result (+ a b))
  (princ (strcat "\nResult: " (itoa result)))
  (princ))

;; -*- coding: utf-8 -*-

;; ------------------------------------------------------
;; ------------------------------------------------------
;; -- MATRIZES TESTE

(setf matriz_0 (make-array '(3 3)
   :initial-contents '((0 0 0) (0 0 0) (0 0 0)))
)
(setf matriz_1 (make-array '(3 3)
   :initial-contents '((5 2 5) (3 0 1) (4 3 4)))
)
(setf matriz_2 (make-array '(4 4)
   :initial-contents '((4 3 3 0) (7 3 3 2) (5 3 3 2) (3 1 3 0) ))
)

;; ------------------------------------------------------
;; ------------------------------------------------------

(defun main ()
    (write-line (write-to-string matriz_0))
    (write-line (write-to-string matriz_1))
    (write-line (write-to-string matriz_2))

)
(main)
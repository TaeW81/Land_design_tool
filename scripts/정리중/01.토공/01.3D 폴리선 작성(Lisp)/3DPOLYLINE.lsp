;; 3D Polyline Creator LISP
;; Command: 3DPOLYLINE
;; 1. Select TEXT entities containing numbers (Z coordinates)
;; 2. Select start point
;; 3. Create closed 3D polyline connecting points in order

(defun c:3DPOLYLINE ( / oldCE ss i ent txt z pt points startPt sortedPts)
  (vl-load-com)
  
  ;; Silent execution
  (setq oldCE (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (defun _reset () (setvar "CMDECHO" oldCE))
  (defun *error* (m) (_reset) (if m (princ (strcat "\nError: " m))) (princ))

  ;; Step 1: Select TEXT entities
  (princ "\nSelect text entities containing numbers: ")
  (setq ss (ssget '((0 . "TEXT"))))
  (if (not ss)
    (progn
      (princ "\nSelection cancelled.")
      (_reset)
      (princ)
      (exit)
    )
  )

  ;; Step 2: Extract points from TEXT entities
  (setq points '())
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq txt (cdr (assoc 1 (entget ent))))
    (if (and txt (numberp (read txt)))
      (progn
        (setq z (atof txt))
        (setq pt (cdr (assoc 10 (entget ent))))
        (setq points (cons (list (car pt) (cadr pt) z) points))
      )
    )
    (setq i (1+ i))
  )

  (if (< (length points) 3)
    (progn
      (princ "\nAt least 3 text entities with numbers are required.")
      (_reset)
      (princ)
      (exit)
    )
  )

  ;; Step 3: Select start point
  (setq startPt (getpoint "\nSelect start point: "))
  (if (not startPt)
    (progn
      (princ "\nCommand cancelled.")
      (_reset)
      (princ)
      (exit)
    )
  )

  ;; Step 4: Find nearest point to start point
  (setq startPt (list (car startPt) (cadr startPt) (if (caddr startPt) (caddr startPt) 0.0)))
  (setq sortedPts (sort-by-distance points startPt))

  (if (< (length sortedPts) 3)
    (progn
      (princ "\nStart point not found.")
      (_reset)
      (princ)
      (exit)
    )
  )

  ;; Step 5: Create 3D polyline
  (command "_.3DPOLY")
  (foreach pt sortedPts
    (command pt)
  )
  ;; Close polyline by adding first point
  (command (car sortedPts))
  (command "")

  (princ "\n✅ 3D closed polyline created successfully.")
  (_reset)
  (princ)
)

;; Sort points by distance from start point
(defun sort-by-distance (points startPt / result remaining current nearest minDist dist pt)
  (setq result '())
  (setq remaining (copy-list points))
  
  ;; Find nearest point to start point
  (setq current (car remaining))
  (setq minDist (distance-3d startPt current))
  (foreach pt (cdr remaining)
    (setq dist (distance-3d startPt pt))
    (if (< dist minDist)
      (progn
        (setq minDist dist)
        (setq current pt)
      )
    )
  )
  
  ;; Remove current from remaining and add to result
  (setq remaining (remove-item current remaining))
  (setq result (cons current result))
  
  ;; Sort remaining points by distance
  (while remaining
    (setq nearest (car remaining))
    (setq minDist (distance-3d current nearest))
    (foreach pt (cdr remaining)
      (setq dist (distance-3d current pt))
      (if (< dist minDist)
        (progn
          (setq minDist dist)
          (setq nearest pt)
        )
      )
    )
    (setq result (cons nearest result))
    (setq remaining (remove-item nearest remaining))
    (setq current nearest)
  )
  
  (reverse result)
)

;; Calculate 3D distance
(defun distance-3d (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt1) (car pt2)))
  (setq dy (- (cadr pt1) (cadr pt2)))
  (setq dz (- (caddr pt1) (caddr pt2)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Remove item from list
(defun remove-item (item lst / result)
  (setq result '())
  (foreach x lst
    (if (not (equal x item 1e-10))
      (setq result (cons x result))
    )
  )
  (reverse result)
)

;; Copy list
(defun copy-list (lst)
  (append lst nil)
)


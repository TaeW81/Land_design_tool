;; Support: LINE/ARC/CIRCLE/ELLIPSE/LWPOLYLINE/POLYLINE/SPLINE
;; Added: Multiple offset lines with automatic termination and block insertion at intersections
(defun c:BFB ( / oldCE sel e typ obj p1 p2 cp par tan rot90 vlen vunit v* v+ v- u half line1 offset_dir offset_dist sin_theta step_len max_dist n all_lines offset_lines acExtendNone new_line crosswalk_pos xline_sel xline_ent xline_data xline_dir)
  (vl-load-com)
  ;; Utilities
(defun rot90 (v) (list (- (cadr v)) (car v) 0.0))
  (defun vlen (v) (sqrt (+ (* (car v)(car v))
                           (* (cadr v)(cadr v))
                           (* (if (nth 2 v) (nth 2 v) 0.0)
                              (if (nth 2 v) (nth 2 v) 0.0)))))
  (defun vunit (v / L) (setq L (vlen v))
    (if (> L 1e-8) (mapcar '(lambda (x) (/ x L)) v) '(1 0 0)))
(defun v* (v s) (mapcar '(lambda (x) (* x s)) v))
(defun v+ (a b) (mapcar '+ a b))
(defun v- (a b) (mapcar '- a b))

  ;; ActiveX extend option
  (setq acExtendNone 0)

  ;; Silent execution
  (setq oldCE (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (defun _reset () (setvar "CMDECHO" oldCE))
  (defun *error* (m) (_reset) (if m (princ (strcat "\nError: " m))) (princ))

  (initget "T")
  (setq sel (entsel "\nSelect curve /Block Setting(T): "))
  (if (= sel "T")
    (progn
      (princ "\nBlock Setting Mode:")
      (setq block_path (getfiled "Select Block File" "" "dwg" 0))
      (if block_path
        (progn
          (princ (strcat "\nBlock file selected: " block_path))
          (initget "T")
          (setq sel (entsel "\nSelect curve /Block Setting(T): "))
        )
        (progn
          (princ "\nNo block file selected. Using default.")
          (setq sel (entsel "\nSelect curve /Block Setting(T): "))
        )
      )
    )
  )
  (if (and sel (setq e (car sel))
           (setq typ (cdr (assoc 0 (entget e))))
           (wcmatch typ "LINE,ARC,CIRCLE,ELLIPSE,LWPOLYLINE,POLYLINE,SPLINE"))
    (progn
      (setq obj (vlax-ename->vla-object e))
      (setq p1 (getpoint "\nSpecify first point on curve: "))
      (setq cp  (vlax-curve-getClosestPointTo obj p1))
      
      ;; Xline selection for direction
      (setq xline_sel (entsel "\nSelect Xline for direction: "))
      (if (and xline_sel (setq xline_ent (car xline_sel))
               (= (cdr (assoc 0 (entget xline_ent))) "XLINE"))
        (progn
          ;; Extract Xline direction vector
          (setq xline_data (entget xline_ent))
          (setq xline_dir (cdr (assoc 11 xline_data)))  ;; Direction vector
          (setq u (vunit xline_dir))                    ;; Unit direction vector
          (princ (strcat "\nXline direction: " (rtos (car u) 2 3) ", " (rtos (cadr u) 2 3)))
        )
        (progn
          (princ "\nInvalid Xline selection. Using perpendicular direction.")
          (setq par (vlax-curve-getParamAtPoint   obj cp))
          (setq tan (vlax-curve-getFirstDeriv     obj par))
          (setq u   (vunit (rot90 tan)))          ;; Perpendicular unit vector
        )
      )
      (setq half 10.0)                        ;; Total length 20 (half = 10)
      (setq all_lines (list))                 ;; Store all created lines
      (setq offset_lines (list))              ;; Store only offset-created lines

      ;; Create first perpendicular line
      (setq line1 (entmakex (list (cons 0  "LINE")
                                  (cons 8  (getvar "CLAYER"))
                                  (cons 10 (v+ cp (v* u half)))
                                  (cons 11 (v+ cp (v* u (- half)))))))
      (setq all_lines (cons line1 all_lines))

      ;; Second point selection for offset direction
      (setq p2 (getpoint "\nSpecify second point for offset direction: "))
      (setq offset_dir (vunit (v- p2 cp)))   ;; Direction from curve point to second point
      (setq max_dist (vlen (v- p2 cp)))      ;; Max travel length along offset_dir
      (setq offset_dist 0.3)                 ;; Target perpendicular spacing between lines
      ;; Adjust step length so that perpendicular distance between parallel lines equals offset_dist
      ;; sin_theta = |det(offset_dir, u)| in 2D
      (setq sin_theta (abs (- (* (car offset_dir) (cadr u))
                               (* (cadr offset_dir) (car u)))))
      (setq step_len (if (> sin_theta 1e-8)
                       (/ offset_dist sin_theta)
                       offset_dist))
      (setq n 1)                             ;; Offset counter
      
      ;; Ask for crosswalk position
      (initget "W S")
      (setq crosswalk_pos (getkword "\nWhere is the crosswalk located? (Above or Left → W / Below or Right → S): "))
      (if (null crosswalk_pos) (setq crosswalk_pos "W"))  ;; Default to W if no input

      ;; Create multiple offset lines
      (while (<= (* step_len n) max_dist)
        (setq new_line (entmakex (list (cons 0  "LINE")
                                       (cons 8  (getvar "CLAYER"))
                                       (cons 10 (v+ (v+ cp (v* u half)) (v* offset_dir (* step_len n))))
                                       (cons 11 (v+ (v+ cp (v* u (- half))) (v* offset_dir (* step_len n)))))))
        (setq all_lines (cons new_line all_lines))
        (setq offset_lines (cons new_line offset_lines))
        (setq n (1+ n))
      )

      ;; Insert blocks at all true intersection points (curve  lines)
      (princ "\nInserting blocks at true intersection points...")
      ;; Insert blocks for intersections with all generated lines (base + offsets)
      (insert_blocks_at_intersections e all_lines acExtendNone)

      (princ (strcat "\nComplete: Perpendicular line and " (itoa (1- n)) " offset lines created."))
      (princ (strcat "\nTotal offset distance: " (rtos (* offset_dist (1- n)) 2 2) " units"))
      
      ;; Delete all generated perpendicular lines (base + offsets)
      (foreach ent all_lines
        (if (and ent (entget ent)) (entdel ent))
      )
    )
    (princ "\nNot a supported curve. (LINE/ARC/CIRCLE/ELLIPSE/LWPOLYLINE/POLYLINE/SPLINE)")
  )
  (_reset)
  (princ)
)

;; Convert input (VARIANT safearray or plain list) to list of 3D points
(defun _variant-to-point-list (v / lst pts tmp)
  (cond
    ((null v) (setq lst nil))
    (T
      ;; Try as VARIANT first (exception-safe). If it fails, fall back to list.
      (setq tmp (vl-catch-all-apply
                  '(lambda () (vlax-safearray->list (vlax-variant-value v)))))
      (if (and tmp (not (vl-catch-all-error-p tmp)))
        (setq lst tmp)
        (if (listp v)
          (setq lst v)
          (setq lst nil)
        )
      )
    )
  )
  (setq pts nil)
  (while (and lst (>= (length lst) 3))
    (setq pts (cons (list (car lst) (cadr lst) (caddr lst)) pts)
          lst (cdddr lst))
  )
  (reverse pts)
)

;; Function to insert blocks at intersection points using IntersectWith
(defun insert_blocks_at_intersections (curve_ent line_entities acExt / curve_obj line_obj v iPts ed sp ep ang rotDeg)
  (setq curve_obj (vlax-ename->vla-object curve_ent))
  (foreach line_ent line_entities
    (setq line_obj (vlax-ename->vla-object line_ent))
    ;; compute line angle once (in degrees) using endpoints
    (setq ed (entget line_ent))
    (setq sp (cdr (assoc 10 ed)))
    (setq ep (cdr (assoc 11 ed)))
    (setq ang (if (and sp ep) (angle sp ep) 0.0))
    (setq angDeg (* 180.0 (/ ang pi)))
    (setq rotDeg angDeg)
    ;; clockwise 90-degree adjustment
    (setq rotDeg (- rotDeg 90.0))
    ;; if the line angle is less than 180, add extra 180 degrees
    (if (< angDeg 180.0) (setq rotDeg (+ rotDeg 180.0)))
    ;; Apply crosswalk position adjustment
    (if (= crosswalk_pos "S") (setq rotDeg (+ rotDeg 180.0)))
    (setq v (vlax-invoke line_obj 'IntersectWith curve_obj acExt))
    (if v
      (progn
        (setq iPts (_variant-to-point-list v))
        (foreach p iPts
          ;; Use XY of intersection (ignore Z for 2D)
          (insert_block_at_point (list (car p) (cadr p) 0.0) rotDeg)
        )
      )
    )
  )
)

;; Function to insert block at specific point with rotation (degrees)
(defun insert_block_at_point (point rotDeg / last new)
  ;; Use user-selected block path or default path
  (if (null block_path)
    (setq block_path "D:\\CAD\\Backdata\\Braille blocks.dwg")
  )
  ;; Capture last entity, insert, then rotate the new block around insertion point
  (setq last (entlast))
  (command "_.-INSERT" block_path point 1 1 rotDeg)
  (setq new (entlast))
  new
)
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
      
      ;; Vector calculation completed
      (setq offset_dist 0.3)                 ;; Target perpendicular spacing between lines
      ;; Adjust step length so that perpendicular distance between parallel lines equals offset_dist
      ;; sin_theta = |det(offset_dir, u)| in 2D
      (setq sin_theta (abs (- (* (car offset_dir) (cadr u))
                               (* (cadr offset_dir) (car u)))))
      (setq step_len (if (> sin_theta 1e-8)
                       (/ offset_dist sin_theta)
                       offset_dist))
      (setq n 1)                             ;; Offset counter
      
      ;; No need for crosswalk position selection anymore

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
      (setq inserted_blocks (insert_blocks_at_intersections e all_lines acExtendNone))
      
      ;; Ask user if they want to rotate blocks 180 degrees
      (initget "Y N")
      (setq rotate_180 (getkword "\nRotate blocks 180 degrees? Y/N: "))
      (if (= rotate_180 "Y")
        (progn
          (princ "\nRotating all blocks 180 degrees...")
          (rotate_all_blocks 180.0)
        )
      )

      ;; Ask user if they want to mirror blocks about their offset lines
      (mirror_blocks_along_lines)

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
  (setq inserted_blocks (list))  ;; Store inserted block and line pairs for later operations
  (foreach line_ent line_entities
    (setq line_obj (vlax-ename->vla-object line_ent))
    ;; Use u vector (기준선) angle for block rotation with 90 degree adjustment
    (setq rotDeg (+ (* 180.0 (/ (angle '(0 0 0) u) pi)) 90.0))  ;; Rotation based on u vector direction + 90 degrees
    
    (setq v (vlax-invoke line_obj 'IntersectWith curve_obj acExt))
    (if v
      (progn
        (setq iPts (_variant-to-point-list v))
        (foreach p iPts
          ;; Insert block at intersection point with appropriate reference point
          (setq new_block (insert_block_at_point (list (car p) (cadr p) 0.0) rotDeg))
          (if new_block (setq inserted_blocks (cons (list new_block line_ent) inserted_blocks)))
        )
      )
    )
  )
  inserted_blocks  ;; Return list of inserted block/line pairs
)

;; Function to rotate all inserted blocks by specified angle
(defun rotate_all_blocks (rotation_angle / block_pair block_ent block_obj ins_pt ang_rad count)
  (setq ang_rad (* rotation_angle (/ pi 180.0)))
  (setq count 0)
  (foreach block_pair inserted_blocks
    (setq block_ent (car block_pair))
    (if (and block_ent (entget block_ent))
      (progn
        (setq block_obj (vlax-ename->vla-object block_ent))
        (setq ins_pt (vlax-get block_obj 'InsertionPoint))
        (if (eq (type ins_pt) 'variant)
          (setq ins_pt (vlax-safearray->list (vlax-variant-value ins_pt)))
        )
        (vla-Rotate block_obj (vlax-3d-point ins_pt) ang_rad)
        (setq count (1+ count))
      )
    )
  )
  (princ (strcat "\n" (itoa count) " blocks rotated by " (rtos rotation_angle 2 0) " degrees."))
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

;; Function to mirror blocks along their corresponding offset lines
(defun mirror_blocks_along_lines ( / response block_pair block_ent line_ent line_data start_pt end_pt new_block mirrored_pairs updated_pair)
  (if (null inserted_blocks)
    (princ "\nNo blocks available to mirror.")
    (progn
      (initget "Y N")
      (setq response (getkword "\nMirror blocks about their offset lines? Y/N: "))
      (if (= response "Y")
        (progn
          (setq mirrored_pairs (list))
          (foreach block_pair inserted_blocks
            (setq block_ent (car block_pair))
            (setq line_ent  (cadr block_pair))
            (setq updated_pair nil)
            (if (and block_ent (entget block_ent) line_ent (entget line_ent))
              (progn
                (setq line_data (entget line_ent))
                (setq start_pt (cdr (assoc 10 line_data)))
                (setq end_pt   (cdr (assoc 11 line_data)))
                (if (and start_pt end_pt)
                  (progn
                    (command "_.MIRROR" block_ent "" start_pt end_pt "Y")
                    (setq new_block (entlast))
                    (if (and new_block (entget new_block))
                      (setq updated_pair (list new_block line_ent)))
                  )
                )
              )
            )
            (if updated_pair
              (setq mirrored_pairs (cons updated_pair mirrored_pairs))
              (setq mirrored_pairs (cons block_pair mirrored_pairs))
            )
          )
          (setq inserted_blocks (reverse mirrored_pairs))
          (princ "\nBlocks mirrored about their offset lines.")
        )
      )
    )
  )
)
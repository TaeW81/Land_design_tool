;; Guide Block LISP - 유도블럭 리습
;; Command: GB
;; 1. Select sidewalk polyline
;; 2. Divide polyline into 5 equal parts and create perpendicular lines at division points
;; 3. Select start point and finish point (on polyline)
;; 4. Insert block at start point with rotation based on start->finish vector

;; Utilities (must be defined before use)
(defun rot90 (v) (list (- (cadr v)) (car v) 0.0))
(defun vlen (v) (sqrt (+ (* (car v)(car v))
                         (* (cadr v)(cadr v))
                         (* (if (nth 2 v) (nth 2 v) 0.0)
                            (if (nth 2 v) (nth 2 v) 0.0)))))
(defun vunit (v / L) 
  (setq L (vlen v))
  (if (> L 1e-8) (mapcar '(lambda (x) (/ x L)) v) '(1 0 0))
)
(defun v* (v s) (mapcar '(lambda (x) (* x s)) v))
(defun v+ (a b) (mapcar '+ a b))
(defun v- (a b) (mapcar '- a b))

;; Function to divide polyline into 5 equal parts and return division points
(defun divide_polyline_5 (polyline_ent / polyline_obj end_param total_length step_length div_pts i dist pt)
  (setq polyline_obj (vlax-ename->vla-object polyline_ent))
  (setq end_param (vlax-curve-getEndParam polyline_obj))
  (setq total_length (vlax-curve-getDistAtParam polyline_obj end_param))
  (setq step_length (/ total_length 5.0))
  
  (setq div_pts (list))
  
  ;; Get 4 division points (excluding start and end)
  ;; Points at 1/5, 2/5, 3/5, 4/5 of total length
  (setq i 1)
  (while (<= i 4)
    (setq dist (* step_length i))
    (setq pt (vlax-curve-getPointAtDist polyline_obj dist))
    (if pt
      (setq div_pts (cons pt div_pts))
    )
    (setq i (1+ i))
  )
  
  (reverse div_pts)  ;; Return points in order
)

;; Function to create perpendicular lines at division points
(defun create_perpendicular_lines (polyline_ent div_pts / polyline_obj perp_lines pt param tan dir perp_dir half_len line_ent p1 p2)
  (setq polyline_obj (vlax-ename->vla-object polyline_ent))
  (setq perp_lines (list))
  (setq half_len 2.5)  ;; Half of 5 (total length = 5)
  
  (foreach pt div_pts
    (setq param (vlax-curve-getParamAtPoint polyline_obj pt))
    (setq tan (vlax-curve-getFirstDeriv polyline_obj param))
    (setq dir (vunit tan))
    (setq perp_dir (rot90 dir))  ;; Perpendicular direction
    
    ;; Calculate line endpoints (centered on division point)
    (setq p1 (v+ pt (v* perp_dir half_len)))
    (setq p2 (v- pt (v* perp_dir half_len)))
    
    ;; Create line
    (setq line_ent (entmakex (list (cons 0  "LINE")
                                   (cons 8  (getvar "CLAYER"))
                                   (cons 10 p1)
                                   (cons 11 p2))))
    (if line_ent
      (setq perp_lines (cons line_ent perp_lines))
    )
  )
  
  perp_lines  ;; Return list of created line entities
)

;; Function to insert guide blocks repeatedly
(defun insert_guide_blocks (polyline_obj start_pt finish_pt / dir_vec dir_unit angle_deg base_angle current_pt count polyline_length dist_from_start dot1_file guide_file)
  ;; Calculate direction vector from start to finish
  (setq dir_vec (v- finish_pt start_pt))
  (setq dir_unit (vunit dir_vec))
  (setq base_angle (* 180.0 (/ (angle start_pt finish_pt) pi)))
  ;; Add 90 degrees clockwise (subtract 90 degrees)
  (setq angle_deg (- base_angle 90.0))
  
  ;; Calculate polyline length from start_pt to finish_pt
  ;; Use absolute distance along the polyline
  (setq dist_from_start (vlax-curve-getDistAtPoint polyline_obj start_pt))
  (setq dist_to_finish (vlax-curve-getDistAtPoint polyline_obj finish_pt))
  (setq polyline_length (abs (- dist_to_finish dist_from_start)))
  
  ;; Debug message
  (princ (strcat "\nPolyline length: " (rtos polyline_length 2 2)))
  
  ;; Set block file paths - use saved global paths if available, otherwise use defaults
  ;; Once set, the paths are preserved until explicitly changed via T option
  (if *GB_dot1_path*
    (setq dot1_file *GB_dot1_path*)
    (setq dot1_file "D:\\CAD\\Backdata\\dot1.dwg")
  )
  
  (if *GB_guide_path*
    (setq guide_file *GB_guide_path*)
    (setq guide_file "D:\\CAD\\Backdata\\guide.dwg")
  )
  
  ;; Insert first block (dot1.dwg) at start point
  (setq current_pt start_pt)
  (setq count 1)
  
  ;; Check if block file exists
  (if (findfile dot1_file)
    (progn
      (command "_.-INSERT" dot1_file current_pt 1 1 angle_deg)
      (princ (strcat "\nInserted dot1 block #" (itoa count)))
    )
    (progn
      (princ (strcat "\nError: Block file not found: " dot1_file))
      (setq count 9999)  ;; Prevent further execution
    )
  )
  
  ;; Insert guide.dwg blocks repeatedly
  ;; Condition: (0.3 * count) > (polyline_length - 0.3)
  ;; Continue while: (0.3 * count) <= (polyline_length - 0.3)
  (if (< count 9999)  ;; Only continue if dot1 block was inserted successfully
    (progn
      (setq count 2)  ;; Next block is #2
      (princ (strcat "\nStarting guide block insertion. Condition: (0.3 * count) <= (" (rtos polyline_length 2 2) " - 0.3)"))
      (while (and (< count 9999) (<= (* 0.3 count) (- polyline_length 0.3)))
        (setq current_pt (v+ start_pt (v* dir_unit (* 0.3 (- count 1)))))
        
        ;; Debug message
        (princ (strcat "\nAttempting to insert guide block #" (itoa count) " at distance: " (rtos (* 0.3 (- count 1)) 2 2)))
        
        ;; Check if block file exists
        (if (findfile guide_file)
          (progn
            (command "_.-INSERT" guide_file current_pt 1 1 angle_deg)
            (princ (strcat "\nInserted guide block #" (itoa count)))
          )
          (progn
            (princ (strcat "\nError: Block file not found: " guide_file))
            (setq count 9999)  ;; Exit loop
          )
        )
        (setq count (1+ count))
      )
      (if (>= count 9999)
        (princ "\nGuide block insertion stopped due to error.")
        (princ (strcat "\nGuide block insertion loop ended. Final count: " (itoa count) ", Condition: (0.3 * " (itoa count) ") = " (rtos (* 0.3 count) 2 2) " > (" (rtos polyline_length 2 2) " - 0.3) = " (rtos (- polyline_length 0.3) 2 2))))
    )
  )
  
  (if (< count 9999)
    (princ (strcat "\nTotal " (itoa (- count 1)) " blocks inserted."))
    (princ "\nBlock insertion cancelled due to error.")
  )
)

(defun c:GB ( / oldCE sel e typ obj polyline_obj start_pt finish_pt div_pts perp_lines)
  (vl-load-com)

  ;; Silent execution
  (setq oldCE (getvar "CMDECHO")) 
  (setvar "CMDECHO" 0)
  (defun _reset () (setvar "CMDECHO" oldCE))
  (defun *error* (m) (_reset) (if m (princ (strcat "\nError: " m))) (princ))

  ;; Block path setting (D for dot1.dwg, G for guide.dwg)
  (initget "T")
  (setq sel (entsel "\nSelect sidewalk polyline /Block Setting(T): "))
  (if (= sel "T")
    (progn
      (princ "\nBlock Setting Mode:")
      (initget "D G")
      (setq block_setting (getkword "\nSelect block type - Dot1(D) / Guide(G): "))
      (if (= block_setting "D")
        (progn
          (setq *GB_dot1_path* (getfiled "Select Dot1 Block File (dot1.dwg)" "" "dwg" 0))
          (if *GB_dot1_path*
            (princ (strcat "\nDot1 block file selected: " *GB_dot1_path*))
            (princ "\nNo block file selected. Using default.")
          )
        )
        (if (= block_setting "G")
          (progn
            (setq *GB_guide_path* (getfiled "Select Guide Block File (guide.dwg)" "" "dwg" 0))
            (if *GB_guide_path*
              (princ (strcat "\nGuide block file selected: " *GB_guide_path*))
              (princ "\nNo block file selected. Using default.")
            )
          )
        )
      )
      (initget "T")
      (setq sel (entsel "\nSelect sidewalk polyline /Block Setting(T): "))
    )
  )

  ;; Check if selection is valid polyline
  (if (and sel (setq e (car sel))
           (setq typ (cdr (assoc 0 (entget e))))
           (wcmatch typ "LWPOLYLINE,POLYLINE"))
    (progn
      (setq polyline_obj (vlax-ename->vla-object e))
      
      ;; Step 3: Divide polyline into 5 equal parts and create perpendicular lines
      (princ "\nDividing polyline into 5 equal parts...")
      (setq div_pts (divide_polyline_5 e))
      (setq perp_lines (create_perpendicular_lines e div_pts))
      
      (princ (strcat "\nCreated " (itoa (length perp_lines)) " perpendicular lines."))
      
      ;; Step 4: Select start point and finish point
      (setq start_pt (getpoint "\nSelect start point: "))
      (if start_pt
        (progn
          ;; Find closest point on polyline to start_pt
          (setq start_pt (vlax-curve-getClosestPointTo polyline_obj start_pt))
          (setq finish_pt (getpoint start_pt "\nSelect finish point (on polyline): "))
          (if finish_pt
            (progn
              ;; Find closest point on polyline to finish_pt
              (setq finish_pt (vlax-curve-getClosestPointTo polyline_obj finish_pt))
              
              ;; Step 5: Insert blocks repeatedly
              (insert_guide_blocks polyline_obj start_pt finish_pt)
              
              (princ "\nGuide blocks inserted successfully.")
            )
            (princ "\nFinish point selection cancelled.")
          )
        )
        (princ "\nStart point selection cancelled.")
      )
      
      ;; Delete perpendicular lines
      (foreach line perp_lines
        (if (and line (entget line)) (entdel line))
      )
      (princ "\nPerpendicular lines deleted.")
    )
    (princ "\nNot a valid polyline. (LWPOLYLINE or POLYLINE required)")
  )
  
  (_reset)
  (princ)
)


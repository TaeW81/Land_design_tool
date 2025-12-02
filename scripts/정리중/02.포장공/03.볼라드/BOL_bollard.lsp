;; Bollard Installation LISP - 볼라드 설치 리습
;; Command: BOL
;; 1. Select two points (A, B) to create polyline
;; 2. Copy polyline from A to B (A', B')
;; 3. Insert bollard block at B' and rotate
;; 4. Copy polyline A'B' from A' to B' (A'', B'')
;; 5. Insert dot(bollard) block at B'' and rotate
;; 6. Delete all created polylines

(defun c:BOL ( / oldCE ptA ptB polyline_ent polyline_obj polyline_copy1 ptA1 ptB1 polyline_copy2 ptA2 ptB2 ptB1_calculated ptB2_calculated)
  (vl-load-com)
  
  ;; Utilities
  (defun vlen (v) (sqrt (+ (* (car v)(car v))
                           (* (cadr v)(cadr v))
                           (* (if (nth 2 v) (nth 2 v) 0.0)
                              (if (nth 2 v) (nth 2 v) 0.0)))))
  (defun vunit (v / L) 
    (setq L (vlen v))
    (if (> L 1e-8) (mapcar '(lambda (x) (/ x L)) v) '(1 0 0))
  )

  ;; Silent execution
  (setq oldCE (getvar "CMDECHO")) 
  (setvar "CMDECHO" 0)
  (defun _reset () (setvar "CMDECHO" oldCE))
  (defun *error* (m) (_reset) (if m (princ (strcat "\nError: " m))) (princ))

  ;; Block path setting (same as previous LISPs)
  (initget "T")
  (setq block_setting (getkword "\nBlock Setting(T) / <Continue>: "))
  (if (= block_setting "T")
    (progn
      (princ "\nBlock Setting Mode:")
      (initget "B D")
      (setq block_type (getkword "\nSelect block type - Bollard(B) / Dot(D): "))
      (if (= block_type "B")
        (progn
          (setq *BOL_bollard_path* (getfiled "Select Bollard Block File (볼라드(기본).dwg)" "" "dwg" 0))
          (if *BOL_bollard_path*
            (princ (strcat "\nBollard block file selected: " *BOL_bollard_path*))
            (princ "\nNo block file selected. Using default.")
          )
        )
        (if (= block_type "D")
          (progn
            (setq *BOL_dot_path* (getfiled "Select Dot Block File (dot(볼라드).dwg)" "" "dwg" 0))
            (if *BOL_dot_path*
              (princ (strcat "\nDot block file selected: " *BOL_dot_path*))
              (princ "\nNo block file selected. Using default.")
            )
          )
        )
      )
    )
  )

  ;; Step 2: Select two points (A, B) to create polyline
  (setq ptA (getpoint "\nSelect first point (A): "))
  (if ptA
    (progn
      (setq ptB (getpoint ptA "\nSelect second point (B): "))
      (if ptB
        (progn
          ;; Create polyline from A to B
          (setq polyline_ent (entmakex (list (cons 0  "LWPOLYLINE")
                                             (cons 100 "AcDbEntity")
                                             (cons 100 "AcDbPolyline")
                                             (cons 90 2)  ; vertex count
                                             (cons 70 0)  ; open polyline
                                             (cons 10 (list (car ptA) (cadr ptA)))
                                             (cons 10 (list (car ptB) (cadr ptB))))))
          
          (if polyline_ent
            (progn
              (princ "\nPolyline created from A to B.")
              (setq polyline_obj (vlax-ename->vla-object polyline_ent))
              
              ;; Step 3: Copy polyline from A to B (A', B')
              ;; Copy using A as base point to B, so A' = B, B' = B + (B - A)
              ;; Copy polyline
              (command "_.COPY" polyline_ent "" ptA ptB)
              (setq polyline_copy1 (entlast))
              
              (if polyline_copy1
                (progn
                  (princ "\nPolyline copied from A to B (A'B').")
                  (setq polyline_copy1_obj (vlax-ename->vla-object polyline_copy1))
                  
                  ;; Step 4: Insert bollard block at B' and rotate
                  ;; Get actual start and end points of copied polyline
                  (setq ptA1 (vlax-curve-getStartPoint polyline_copy1_obj))  ; A' = actual start point
                  (setq ptB1 (vlax-curve-getEndPoint polyline_copy1_obj))    ; B' = actual end point
                  
                  ;; Debug: Check if calculated B' matches actual B'
                  (setq ptB1_calculated (mapcar '+ ptB (mapcar '- ptB ptA)))  ; B' = B + (B - A) = 2*B - A
                  (princ (strcat "\nDebug: A = (" (rtos (car ptA) 2 4) ", " (rtos (cadr ptA) 2 4) ")"))
                  (princ (strcat "\nDebug: B = (" (rtos (car ptB) 2 4) ", " (rtos (cadr ptB) 2 4) ")"))
                  (princ (strcat "\nDebug: A' (actual) = (" (rtos (car ptA1) 2 4) ", " (rtos (cadr ptA1) 2 4) ")"))
                  (princ (strcat "\nDebug: B' (actual) = (" (rtos (car ptB1) 2 4) ", " (rtos (cadr ptB1) 2 4) ")"))
                  (princ (strcat "\nDebug: B' (calculated 2B-A) = (" (rtos (car ptB1_calculated) 2 4) ", " (rtos (cadr ptB1_calculated) 2 4) ")"))
                  
                  (insert_bollard_block ptB1 polyline_copy1_obj)
                  
                  ;; Step 5: Copy polyline A'B' from A' to B' (A'', B'')
                  ;; Copy using A' as base point to B', so A'' = B', B'' = B' + (B' - A')
                  ;; Copy polyline
                  (command "_.COPY" polyline_copy1 "" ptA1 ptB1)
                  (setq polyline_copy2 (entlast))
                  
                  (if polyline_copy2
                    (progn
                      (princ "\nPolyline copied from A' to B' (A''B'').")
                      (setq polyline_copy2_obj (vlax-ename->vla-object polyline_copy2))
                      
                      ;; Step 6: Insert dot(bollard) block at B'' and rotate
                      ;; Get actual start and end points of copied polyline
                      (setq ptA2 (vlax-curve-getStartPoint polyline_copy2_obj))  ; A'' = actual start point
                      (setq ptB2 (vlax-curve-getEndPoint polyline_copy2_obj))    ; B'' = actual end point
                      
                      ;; Debug: Check if B'' is at the correct position
                      (setq ptB2_calculated (mapcar '+ ptB1 (mapcar '- ptB1 ptA1)))  ; B'' = B' + (B' - A') = 2*B' - A'
                      (princ (strcat "\nDebug: A'' (actual) = (" (rtos (car ptA2) 2 4) ", " (rtos (cadr ptA2) 2 4) ")"))
                      (princ (strcat "\nDebug: B'' (actual) = (" (rtos (car ptB2) 2 4) ", " (rtos (cadr ptB2) 2 4) ")"))
                      (princ (strcat "\nDebug: B'' (calculated 2B'-A') = (" (rtos (car ptB2_calculated) 2 4) ", " (rtos (cadr ptB2_calculated) 2 4) ")"))
                      
                      (insert_dot_block ptB2 polyline_copy2_obj)
                      
                      ;; Step 7: Delete all created polylines
                      (if (and polyline_ent (entget polyline_ent)) (entdel polyline_ent))
                      (if (and polyline_copy1 (entget polyline_copy1)) (entdel polyline_copy1))
                      (if (and polyline_copy2 (entget polyline_copy2)) (entdel polyline_copy2))
                      (princ "\nAll polylines deleted.")
                    )
                    (princ "\nFailed to create second polyline copy.")
                  )
                )
                (princ "\nFailed to create first polyline copy.")
              )
            )
            (princ "\nFailed to create polyline.")
          )
        )
        (princ "\nSecond point selection cancelled.")
      )
    )
    (princ "\nFirst point selection cancelled.")
  )
  
  (_reset)
  (princ)
)

;; Function to insert bollard block at point and rotate based on polyline vector
(defun insert_bollard_block (insert_pt polyline_obj / bollard_file angle_deg param tan dir_vec)
  ;; Use saved global path or default
  (if *BOL_bollard_path*
    (setq bollard_file *BOL_bollard_path*)
    (setq bollard_file "D:\\CAD\\Backdata\\볼라드(기본).dwg")
  )
  
  ;; Calculate rotation angle from polyline direction at end point
  (setq param (vlax-curve-getEndParam polyline_obj))
  (setq tan (vlax-curve-getFirstDeriv polyline_obj param))
  (setq dir_vec (vunit tan))
  (setq angle_deg (- (* 180.0 (/ (atan (cadr dir_vec) (car dir_vec)) pi)) 90.0))  ; Add 90 degrees clockwise
  
  ;; Check if block file exists
  (if (findfile bollard_file)
    (progn
      (command "_.-INSERT" bollard_file insert_pt 1 1 angle_deg)
      (princ (strcat "\nBollard block inserted at B'."))
    )
    (princ (strcat "\nError: Block file not found: " bollard_file))
  )
)

;; Function to insert dot(bollard) block at point and rotate based on polyline vector
(defun insert_dot_block (insert_pt polyline_obj / dot_file angle_deg param tan dir_vec)
  ;; Use saved global path or default
  (if *BOL_dot_path*
    (setq dot_file *BOL_dot_path*)
    (setq dot_file "D:\\CAD\\Backdata\\dot(볼라드).dwg")
  )
  
  ;; Calculate rotation angle from polyline direction at end point
  (setq param (vlax-curve-getEndParam polyline_obj))
  (setq tan (vlax-curve-getFirstDeriv polyline_obj param))
  (setq dir_vec (vunit tan))
  (setq angle_deg (- (* 180.0 (/ (atan (cadr dir_vec) (car dir_vec)) pi)) 90.0))  ; Add 90 degrees clockwise
  
  ;; Check if block file exists
  (if (findfile dot_file)
    (progn
      (command "_.-INSERT" dot_file insert_pt 1 1 angle_deg)
      (princ (strcat "\nDot(bollard) block inserted at B''."))
    )
    (princ (strcat "\nError: Block file not found: " dot_file))
  )
)


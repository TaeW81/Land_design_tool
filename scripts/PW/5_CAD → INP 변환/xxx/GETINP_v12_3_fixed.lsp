;;; GETINP_v12_3_fixed.lsp
;;; EPANET INP File Auto Generation Script
;;; Created: 2024-03-21
;;; Modified: 2025-07-15

(vl-load-com)

;; Message definitions
(setq MSG_SELECT_ALL       "Select all objects: ")
(setq MSG_NO_SELECTION    "No objects selected.")
(setq MSG_NO_OBJECTS      "Both Junction and Pipeline must be selected.")
(setq MSG_JUNCTION_COUNT  "Selected Junction objects: ")
(setq MSG_PIPELINE_COUNT  "Selected Pipeline objects: ")
(setq MSG_COORDINATES     "Writing Coordinates section...")
(setq MSG_PROCESSING      "Processing...")
(setq MSG_COMPLETE        "Completed.")
(setq MSG_FILE_SAVED      "INP file saved: ")
(setq MSG_PIPE_INFO       "Pipe info: ")
(setq MSG_LENGTH          "Length: ")
(setq MSG_VERTICES        "Vertices count: ")
(setq MSG_DATA_SORT       "Sorting data...")

;; String conversion function (Express Tools not required)
(defun convert-string (str)
  str
)

;; Message print function
(defun princ-kr (str)
  (princ str)
)

(defun C:GETINP ( / fname selected-ss junction-list pipeline-list file)
  ;; Select all objects
  (setq selected-ss (ssget))
  (if (null selected-ss)
      (progn
        (princ "\nNo objects selected.")
        (exit)
      )
  )
  
  ;; Sort selected objects by type
  (setq junction-list '()
        pipeline-list '())
  
  (setq i 0)
  (repeat (sslength selected-ss)
    (setq ent (ssname selected-ss i))
    (setq ent-data (entget ent))
    (cond 
      ;; Junction check
      ((and (= "TEXT" (cdr (assoc 0 ent-data)))
            (= "L_Junction" (cdr (assoc 8 ent-data))))
       (setq junction-list (cons ent junction-list)))
      ;; Pipeline check
      ((and (= "LWPOLYLINE" (cdr (assoc 0 ent-data)))
            (= "L_pipeline" (cdr (assoc 8 ent-data))))
       (setq pipeline-list (cons ent pipeline-list)))
    )
    (setq i (1+ i))
  )
  
  ;; Check list validity
  (if (or (null junction-list) (null pipeline-list))
      (progn
        (princ "\nBoth Junction and Pipeline must be selected.")
        (exit)
      )
  )
  
  ;; Create selection sets
  (setq junction-ss (ssadd))
  (foreach ent junction-list
    (ssadd ent junction-ss))
  
  (setq pipeline-ss (ssadd))
  (foreach ent pipeline-list
    (ssadd ent pipeline-ss))
  
  ;; File setup
  (setq dwg-path (getvar "DWGPREFIX"))
  (setq fname (vl-filename-base (getvar "DWGNAME")))
  (setq full-path (strcat dwg-path fname ".inp"))
  (setq file (open full-path "w"))
  
  ;; [TITLE]
  (write-line "[TITLE]" file)
  (write-line fname file)
  (write-line "" file)
  
  ;; [JUNCTIONS] & [RESERVOIRS]
  (write-junctions-and-reservoirs file junction-ss)
  
  ;; [COORDINATES]
  (write-coordinates file junction-ss)
  
  ;; [PIPES] & [VERTICES]
  (write-pipes-and-vertices file pipeline-ss)

  ;; [TIMES] section
  (write-line "[TIMES]" file)
  (write-line " Duration           \t0:00" file)
  (write-line " Hydraulic Timestep \t1:00" file)
  (write-line " Quality Timestep   \t0:05" file)
  (write-line " Pattern Timestep   \t1:00" file)
  (write-line " Pattern Start      \t0:00" file)
  (write-line " Report Timestep    \t1:00" file)
  (write-line " Report Start       \t0:00" file)
  (write-line " Start ClockTime    \t12:00 AM" file)
  (write-line " Statistic          \tNONE" file)
  (write-line "" file)
  
  ;; [OPTIONS] section
  (write-line "[OPTIONS]" file)
  (write-line " Units              \tCMD" file)
  (write-line " Headloss           \tH-W" file)
  (write-line " Specific Gravity   \t1" file)
  (write-line " Viscosity          \t1" file)
  (write-line " Trials             \t500" file)
  (write-line " Accuracy           \t0.001" file)
  (write-line " CHECKFREQ          \t2" file)
  (write-line " MAXCHECK           \t10" file)
  (write-line " DAMPLIMIT          \t0" file)
  (write-line " Unbalanced         \tContinue 10" file)
  (write-line " Pattern            \t1" file)
  (write-line " Demand Multiplier  \t1.5" file)
  (write-line " Emitter Exponent   \t0.5" file)
  (write-line " Quality            \tNone mg/L" file)
  (write-line " Diffusivity        \t1" file)
  (write-line " Tolerance          \t0.01" file)
  (write-line "" file)
  
  ;; Close file
  (close file)
  (princ "\n=========================================")
  (princ "\n          (c)2025 KUNHWA Corp.")
  (princ "\n            Created by TW")
  (princ "\n=========================================")
  (princ)
)

;; Write Junction and Reservoir sections to file
(defun write-junctions-and-reservoirs (file selected-ss / ss id elev junctions reservoirs i ent pt)
  (setq ss selected-ss)
  (if ss
    (progn
      (setq junctions '()
            reservoirs '())
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq id (cdr (assoc 1 (entget ent))))
        (setq pt (cdr (assoc 10 (entget ent))))
        (setq elev (get_text_at_point pt "L_FH"))
        
        (if (= id "1")
            (setq reservoirs (cons (list id elev) reservoirs))
            (setq junctions (cons (list id elev "0" "") junctions))
        )
        (setq i (1+ i))
      )
      
      (if junctions
          (progn
            (setq junctions 
                  (vl-sort junctions 
                          '(lambda (a b) 
                             (< (strcase (car a)) (strcase (car b))))))
            
            (write-line "[JUNCTIONS]" file)
            (write-line ";ID              Elev            Demand" file)
            (foreach junction junctions
              (write-line (strcat (nth 0 junction) "               "
                                 (nth 1 junction) "              "
                                 (nth 2 junction))
                         file)
            )
            (write-line "" file)
          )
      )
      
      (if reservoirs
          (progn
            (write-line "[RESERVOIRS]" file)
            (write-line ";ID              Head" file)
            (foreach reservoir reservoirs
              (write-line (strcat (nth 0 reservoir) "               "
                                 (nth 1 reservoir))
                         file)
            )
            (write-line "" file)
          )
      )
    )
  )
)

;; Write Coordinates section to file
(defun write-coordinates (file selected-ss / ss obj txt pt x y i coord-list)
  (setq ss selected-ss)
  (if ss
    (progn
      (write-line "[COORDINATES]" file)
      (write-line ";Node            X-Coord         Y-Coord" file)

      ;; Collect all coordinates first
      (setq coord-list '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (ssname ss i))
        (setq obj-data (entget obj))
        ;; Only process TEXT objects on L_Junction layer
        (if (and (= (cdr (assoc 0 obj-data)) "TEXT")
                 (= (cdr (assoc 8 obj-data)) "L_Junction"))
          (progn
            (setq txt (cdr (assoc 1 obj-data)))    ; Text content
            (setq pt (cdr (assoc 10 obj-data)))    ; Coordinates
            (setq x (rtos (car pt) 2 5))           ; X coordinate (5 decimal places)
            (setq y (rtos (cadr pt) 2 5))          ; Y coordinate (5 decimal places)
            (setq coord-list (cons (list txt x y) coord-list))
          )
        )
        (setq i (1+ i))
      )
      
      ;; Sort coordinates by node name
      (setq coord-list (vl-sort coord-list 
                        '(lambda (a b) 
                           (< (strcase (car a)) (strcase (car b))))))
      
      ;; Write sorted coordinates
      (foreach coord coord-list
        (write-line (strcat (nth 0 coord) "               " 
                           (nth 1 coord) "         " 
                           (nth 2 coord)) file)
      )
      (write-line "" file)
    )
  )
)

;; Extract text at specific coordinates
(defun get_text_at_point (pt layer / ss txt)
  (setq ss (ssget "C" 
                  (list (- (car pt) 2.0) (- (cadr pt) 2.0)) 
                  (list (+ (car pt) 2.0) (+ (cadr pt) 2.0)) 
                  (list '(0 . "TEXT") (cons 8 layer))))
  (if ss
    (setq txt (cdr (assoc 1 (entget (ssname ss 0)))))
    (setq txt "N/A"))
  txt
)

;; Extract text near specific coordinates with high precision
(defun get_text_near_point (pt layer / ss txt)
  (setq ss (ssget "C" 
                  (list (- (car pt) 0.1) (- (cadr pt) 0.1)) 
                  (list (+ (car pt) 0.1) (+ (cadr pt) 0.1)) 
                  (list '(0 . "TEXT") (cons 8 layer))))
  (if ss
    (setq txt (cdr (assoc 1 (entget (ssname ss 0))))) ; Return first text object
    (setq txt "N/A")) ; Return "N/A" if no text found
  txt
)

;; Find junction with high precision matching
(defun find_junction_precise (pt / ss txt min-dist best-txt dist search-ranges)
  (setq search-ranges '(0.05 0.1 0.2 0.5 1.0 2.0))
  (setq best-txt "N/A"
        min-dist 999999.0)
  
  (foreach range search-ranges
    (setq ss (ssget "C" 
                    (list (- (car pt) range) (- (cadr pt) range)) 
                    (list (+ (car pt) range) (+ (cadr pt) range)) 
                    (list '(0 . "TEXT") (cons 8 "L_Junction"))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq txt-obj (ssname ss i)
                txt-pt (cdr (assoc 10 (entget txt-obj)))
                txt (cdr (assoc 1 (entget txt-obj)))
                dist (distance pt txt-pt))
          ;; Check for exact match within 0.01 units first
          (if (< dist 0.01)
              (setq best-txt txt
                    min-dist 0.0)
              (if (< dist min-dist)
                  (setq min-dist dist
                        best-txt txt)))
          (setq i (1+ i)))
        ;; If we found a very close junction, use it immediately
        (if (and (not (= best-txt "N/A")) (< min-dist 0.1))
            (setq range 999.0)) ; Exit the loop
      )
    )
  )
  best-txt)

;; Find text with high precision matching (for any layer)
(defun find_text_precise (pt layer / ss txt min-dist best-txt dist search-ranges)
  (setq search-ranges '(0.05 0.1 0.2 0.5 1.0 2.0))
  (setq best-txt "N/A"
        min-dist 999999.0)
  
  (foreach range search-ranges
    (setq ss (ssget "C" 
                    (list (- (car pt) range) (- (cadr pt) range)) 
                    (list (+ (car pt) range) (+ (cadr pt) range)) 
                    (list '(0 . "TEXT") (cons 8 layer))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq txt-obj (ssname ss i)
                txt-pt (cdr (assoc 10 (entget txt-obj)))
                txt (cdr (assoc 1 (entget txt-obj)))
                dist (distance pt txt-pt))
          ;; Check for exact match within 0.01 units first
          (if (< dist 0.01)
              (setq best-txt txt
                    min-dist 0.0)
              (if (< dist min-dist)
                  (setq min-dist dist
                        best-txt txt)))
          (setq i (1+ i)))
        ;; If we found a very close text, use it immediately
        (if (and (not (= best-txt "N/A")) (< min-dist 0.1))
            (setq range 999.0)) ; Exit the loop
      )
    )
  )
  best-txt)

;; Check if two points are equal within 5 decimal places precision
(defun points_equal_5dec (pt1 pt2 / x1 y1 x2 y2)
  (setq x1 (car pt1) y1 (cadr pt1)
        x2 (car pt2) y2 (cadr pt2))
  (and (< (abs (- x1 x2)) 0.00001)
       (< (abs (- y1 y2)) 0.00001)))

;; Check if vertex is duplicate in vertices list (5-decimal precision)
(defun is_duplicate_vertex (vtx vertices_list / found)
  (setq found nil)
  (foreach existing vertices_list
    (if (and (listp existing) 
             (>= (length existing) 3)
             (points_equal_5dec vtx (list (atof (nth 1 existing)) 
                                          (atof (nth 2 existing)))))
        (setq found t)))
  found)

;; Find exact junction at specific coordinates with improved matching
(defun find_exact_junction (pt / ss txt min-dist best-txt best-pt dist)
  ;; Try multiple search ranges
  (setq search-ranges '(0.5 1.0 2.0 5.0 10.0))
  (setq best-txt "N/A"
        min-dist 999999.0)
  
  (foreach range search-ranges
    (setq ss (ssget "C" 
                    (list (- (car pt) range) (- (cadr pt) range)) 
                    (list (+ (car pt) range) (+ (cadr pt) range)) 
                    (list '(0 . "TEXT") (cons 8 "L_Junction"))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq txt-obj (ssname ss i)
                txt-pt (cdr (assoc 10 (entget txt-obj)))
                txt (cdr (assoc 1 (entget txt-obj)))
                dist (distance pt txt-pt))
          ;; Check for exact match within 5 decimal places first
          (if (points_equal_5dec pt txt-pt)
              (setq best-txt txt
                    min-dist 0.0)
              (if (< dist min-dist)
                  (setq min-dist dist
                        best-txt txt)))
          (setq i (1+ i)))
        ;; If we found a junction within reasonable distance, use it
        (if (and (not (= best-txt "N/A")) (< min-dist 5.0))
            (setq range 999.0)) ; Exit the loop
      )
    )
  )
  best-txt)

;; Get vertices from specific polyline object (v11 approach)
(defun get_polyline_vertices (pline / num-verts vertices i pt)
  (setq num-verts (vlax-curve-getendparam pline))
  (setq vertices '())
  (setq i 1) ; Start from 1 to skip first vertex (start point)
  (while (< i num-verts)
    (setq pt (vlax-curve-getpointatparam pline i))
    (if (not (member pt vertices)) ; Avoid duplicates
      (setq vertices (cons pt vertices)))
    (setq i (1+ i)))
  vertices ; Return vertices list
)

;; Check if pipe ID is already used
(defun is_pipe_id_used (id used_ids)
  (member id used_ids))

;; Generate unique pipe ID
(defun generate_unique_pipe_id (base_id used_ids / counter new_id)
  (setq counter 1)
  (setq new_id base_id)
  (while (is_pipe_id_used new_id used_ids)
    (setq new_id (strcat base_id "_" (itoa counter)))
    (setq counter (1+ counter)))
  new_id)

;; Write Pipes and Vertices sections to file (based on v11)
(defun write-pipes-and-vertices (file pipeline-ss / pline id node1 node2 len dia data_list vertices_list)
  (vl-load-com)
  (princ-kr MSG_PIPE_INFO)
  
  (princ-kr (strcat MSG_PIPELINE_COUNT (itoa (sslength pipeline-ss))))
  
  (if pipeline-ss
    (progn
      (setq data_list '() vertices_list '())
      (setq i 0)
      (repeat (sslength pipeline-ss)
        (setq pline (ssname pipeline-ss i))
        (princ-kr (strcat MSG_PIPE_INFO (itoa (1+ i))))
        
        (setq len (vlax-curve-getdistatparam pline (vlax-curve-getendparam pline)))
        (setq midpt (vlax-curve-getpointatdist pline (/ len 2.0)))
        (princ-kr (strcat MSG_LENGTH (rtos len 2 2)))

        ;; Get ID (using precise search)
        (setq id (find_text_precise midpt "L_Pipes"))
        (princ-kr (strcat MSG_PIPE_INFO id))

        ;; Get Node1 (using precise junction matching)
        (setq startpt (vlax-curve-getstartpoint pline))
        (setq node1 (find_junction_precise startpt))
        (princ-kr (strcat MSG_PIPE_INFO node1))

        ;; Get Node2 (using precise junction matching)
        (setq endpt (vlax-curve-getendpoint pline))
        (setq node2 (find_junction_precise endpt))
        (princ-kr (strcat MSG_PIPE_INFO node2))

        ;; Get Diameter (using precise search)
        (setq dia (find_text_precise midpt "L_diameter"))
        (princ-kr (strcat MSG_PIPE_INFO dia))

        ;; Add to data list (no filtering - like v11)
        (setq data_list 
              (cons (list id node1 node2 (rtos len 2 2) dia "110" "0" "Open") data_list))

        ;; Process VERTICES (simplified like v11)
        (setq vertices (get_polyline_vertices pline))
        (princ-kr (strcat MSG_VERTICES (itoa (length vertices))))
        (foreach vtx vertices
          (setq vertices_list (cons (list id 
                                        (rtos (car vtx) 2 12) 
                                        (rtos (cadr vtx) 2 12)) 
                                  vertices_list)))

        (setq i (1+ i))
      )

      ;; Sort by ID (simple like v11)
      (setq data_list (vl-sort data_list '(lambda (a b) (< (car a) (car b)))))
      (setq vertices_list (vl-sort vertices_list '(lambda (a b) (< (car a) (car b)))))

      (princ-kr MSG_DATA_SORT)
      
      ;; [PIPES] section
      (write-line "[PIPES]" file)
      (write-line ";ID              Node1           Node2           Length          Diameter        Roughness       MinorLoss       Status" file)
      (foreach data data_list
        (write-line (strcat (nth 0 data) "               "
                           (nth 1 data) "              "
                           (nth 2 data) "              "
                           (nth 3 data) "              "
                           (nth 4 data) "              "
                           (nth 5 data) "              "
                           (nth 6 data) "              "
                           (nth 7 data))
                   file)
      )
      (write-line "" file)

      ;; [VERTICES] section
      (write-line "[VERTICES]" file)
      (write-line ";Link            X-Coord         Y-Coord" file)
      (foreach vtx vertices_list
        (write-line (strcat (nth 0 vtx) "               "
                           (nth 1 vtx) "         "
                           (nth 2 vtx))
                   file)
      )
      (write-line "" file)
      (princ-kr MSG_COMPLETE)
    )
    (princ-kr "\nNo Pipeline objects found.")
  )
)

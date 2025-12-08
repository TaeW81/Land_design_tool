;;; GETINP_v12_3.lsp
;;; EPANET INP File Auto Generation Script
;;; Created: 2024-03-21
;;; Modified: 2025-07-15

(vl-load-com)
;; Express Tools dependency removed

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

;; String conversion function
(defun convert-string (str)
  str  ; Simple passthrough without acet functions
)

;; Korean message print function
(defun princ-kr (str)
  (princ str)
)

(defun C:GETINP (/ selected-ss junction-list pipeline-list
                     junction-ss pipeline-ss file dwg-path fname
                     full-path i ent ent-data)
  ;; Select all objects
  (setq selected-ss (ssget))
  (if (null selected-ss)
      (progn (princ "\nNo objects selected.") (exit)))

  ;; Sort by type
  (setq junction-list '() pipeline-list '())
  (setq i 0)
  (repeat (sslength selected-ss)
    (setq ent      (ssname selected-ss i)
          ent-data (entget ent))
    (cond
      ((and (= "TEXT" (cdr (assoc 0 ent-data)))
            (= "L_Junction" (cdr (assoc 8 ent-data))))
       (setq junction-list (cons ent junction-list)))
      ((and (= "LWPOLYLINE" (cdr (assoc 0 ent-data)))
            (= "L_pipeline" (cdr (assoc 8 ent-data))))
       (setq pipeline-list (cons ent pipeline-list))))
    (setq i (1+ i)))
  (if (or (null junction-list) (null pipeline-list))
      (progn (princ "\nBoth Junction and Pipeline must be selected.") (exit)))

  ;; Create selection sets
  (setq junction-ss (ssadd))
  (foreach ent junction-list (ssadd ent junction-ss))
  (setq pipeline-ss (ssadd))
  (foreach ent pipeline-list (ssadd ent pipeline-ss))

  ;; File path
  (setq dwg-path (getvar "DWGPREFIX")
        fname     (vl-filename-base (getvar "DWGNAME"))
        full-path (strcat dwg-path fname ".inp")
        file      (open full-path "w"))

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

  ;; [TIMES]
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

  ;; [OPTIONS]
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

;;; Write Junction and Reservoir sections
(defun write-junctions-and-reservoirs (file selected-ss / ss id elev junctions reservoirs i ent pt)
  (setq ss selected-ss i 0 junctions '() reservoirs '())
  (while (< i (sslength ss))
    (setq ent  (ssname ss i)
          id   (cdr (assoc 1 (entget ent)))
          pt   (cdr (assoc 10 (entget ent)))
          elev (get_text_at_point pt "L_FH"))
    (if (= id "1")
        (setq reservoirs (cons (list id elev) reservoirs))
        (setq junctions (cons (list id elev "0" "") junctions)))
    (setq i (1+ i)))
  (setq junctions (vl-sort junctions '(lambda (a b) (< (car a) (car b)))))
  (write-line "[JUNCTIONS]" file)
  (write-line ";ID              Elev            Demand" file)
  (foreach j junctions
    (write-line (strcat (nth 0 j) "               " (nth 1 j) "              " (nth 2 j)) file))
  (write-line "" file)
  (if reservoirs
      (progn
        (write-line "[RESERVOIRS]" file)
        (write-line ";ID              Head" file)
        (foreach r reservoirs
          (write-line (strcat (nth 0 r) "               " (nth 1 r)) file))
        (write-line "" file)))
)

;;; Write Coordinates section
(defun write-coordinates (file selected-ss / ss obj obj-data txt pt x y i)
  (setq ss selected-ss i 0)
  (write-line "[COORDINATES]" file)
  (write-line ";Node            X-Coord         Y-Coord" file)
  (while (< i (sslength ss))
    (setq obj      (ssname ss i)
          obj-data (entget obj))
    (if (and (= (cdr (assoc 0 obj-data)) "TEXT")
             (= (cdr (assoc 8 obj-data)) "L_Junction"))
        (progn
          (setq txt (cdr (assoc 1 obj-data))
                pt  (cdr (assoc 10 obj-data))
                x   (rtos (car pt) 2 12)
                y   (rtos (cadr pt) 2 12))
          (write-line (strcat txt "               " x "         " y) file)))
    (setq i (1+ i)))
  (write-line "" file)
)

;;; Extract text at specific coordinates
(defun get_text_at_point (pt layer / ss txt)
  (setq ss (ssget "C"
                  (list (- (car pt) 2.0) (- (cadr pt) 2.0))
                  (list (+ (car pt) 2.0) (+ (cadr pt) 2.0))
                  (list '(0 . "TEXT") (cons 8 layer))))
  (if ss (setq txt (cdr (assoc 1 (entget (ssname ss 0))))) (setq txt "N/A"))
  txt
)

;;; Extract text near specific coordinates (narrow range)
(defun get_text_near_point (pt layer / ss txt)
  (setq ss (ssget "C"
                  (list (- (car pt) 1) (- (cadr pt) 1))
                  (list (+ (car pt) 1) (+ (cadr pt) 1))
                  (list '(0 . "TEXT") (cons 8 layer))))
  (if ss (setq txt (cdr (assoc 1 (entget (ssname ss 0))))) (setq txt "N/A"))
  txt
)

;;; Calculate polyline vertices
(defun get_polyline_vertices (pline / plobj coords bulges segments verts i pt-start pt-end bulge param-st param-en param-mid pt-mid)
  (setq plobj    (vlax-ename->vla-object pline)
        coords   (vlax-safearray->list (vlax-get plobj 'Coordinates))
        bulges   (vlax-safearray->list (vlax-get plobj 'Bulges))
        segments (length bulges)
        verts    '()
        i        0)
  (while (< i segments)
    (setq pt-start (list (nth (* 2 i) coords) (nth (+ (* 2 i) 1) coords))
          pt-end   (list (nth (* 2 (1+ i)) coords) (nth (+ (* 2 (1+ i)) 1) coords))
          bulge    (nth i bulges))
    (if (/= bulge 0.0)
        (progn
          (setq param-st (vlax-curve-getParamAtPoint plobj pt-start)
                param-en (vlax-curve-getParamAtPoint plobj pt-end))
          (when (< param-en param-st)
            (setq param-en (+ param-en (vlax-curve-getEndParam plobj))))
          (setq param-mid (/ (+ param-st param-en) 2.0)
                pt-mid    (vlax-safearray->list
                            (vlax-curve-getPointAtParam plobj param-mid)))
          (when (and (> i 0) (not (member pt-start verts)))
            (setq verts (cons pt-start verts)))
          (unless (member pt-mid verts)
            (setq verts (cons pt-mid verts)))
          (when (and (< i (1- segments)) (not (member pt-end verts)))
            (setq verts (cons pt-end verts))))
        (when (and (> i 0) (not (member pt-start verts)))
          (setq verts (cons pt-start verts))))
    (setq i (1+ i)))
  verts
)

;;; Write Pipes and Vertices sections
(defun write-pipes-and-vertices (file pipeline-ss / pline plobj data_list vertices_list len midpt id startpt node1 endpt node2 dia i vtx)
  (vl-load-com)
  (if (and pipeline-ss (> (sslength pipeline-ss) 0))
      (progn
        (write-line "[PIPES]" file)
        (write-line ";ID              Node1           Node2           Length          Diameter        Roughness       MinorLoss       Status" file)
        (setq data_list     '() vertices_list '() i 0)
        (while (< i (sslength pipeline-ss))
          (setq pline (ssname pipeline-ss i)
                plobj (vlax-ename->vla-object pline))
          (vl-catch-all-apply (function (lambda ()
                             (setq len     (vlax-curve-getDistAtParam plobj (vlax-curve-getEndParam plobj))
                                   midpt   (vlax-curve-getPointAtDist plobj (/ len 2.0))
                                   id      (get_text_near_point midpt "L_pipes")
                                   startpt (vlax-curve-getStartPoint plobj)
                                   node1   (get_text_near_point startpt "L_Junction")
                                   endpt   (vlax-curve-getEndPoint plobj)
                                   node2   (get_text_near_point endpt "L_Junction")
                                   dia     (get_text_near_point midpt "L_diameter")))))
          (when (and id node1 node2 len dia)
            (setq data_list     (cons (list id node1 node2 (rtos len 2 2) dia "110" "0" "Open") data_list))
            (foreach vtx (get_polyline_vertices pline)
              (when (and (listp vtx) (not (member vtx vertices_list)))
                (setq vertices_list (cons (list id (rtos (car vtx) 2 12) (rtos (cadr vtx) 2 12)) vertices_list)))
            )
          (setq i (1+ i)))
        (setq data_list     (vl-sort data_list '(lambda (a b) (< (car a) (car b))))
              vertices_list (vl-sort vertices_list '(lambda (a b) (< (car a) (car b)))))
        (foreach data data_list
          (write-line (strcat (nth 0 data) "               " (nth 1 data) "              " (nth 2 data) "              " (nth 3 data) "              " (nth 4 data) "              " (nth 5 data) "              " (nth 6 data) "              " (nth 7 data)) file))
        (write-line "" file)
        (write-line "[VERTICES]" file)
        (write-line ";Link            X-Coord         Y-Coord" file)
        (foreach vtx vertices_list
          (write-line (strcat (nth 0 vtx) "               " (nth 1 vtx) "         " (nth 2 vtx)) file))
        (write-line "" file)))

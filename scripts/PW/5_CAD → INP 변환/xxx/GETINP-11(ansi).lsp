;;; GETINP-9.LSP
;;; EPANET INP 파일 자동 생성 스크립트
;;; 작성일: 2024-03-21
;;; 수정일: 2024-03-22

(vl-load-com)

;; ACAD 문자열 변환 유틸리티 로드
(if (not acet-str-to-list)
    (vl-load-comfile "acet.vlx")
)

;; 메시지 상수 정의
(setq MSG_SELECT_ALL       "전체를 선택하세요: ")
(setq MSG_NO_SELECTION    "선택된 객체가 없습니다.")
(setq MSG_NO_OBJECTS      "Junction과 Pipeline이 모두 선택되어야 합니다.")
(setq MSG_JUNCTION_COUNT  "추출된 Junction 객체 수: ")
(setq MSG_PIPELINE_COUNT  "추출된 Pipeline 객체 수: ")
(setq MSG_COORDINATES     "Coordinates 정보를 기록 중...")
(setq MSG_PROCESSING      "처리 중...")
(setq MSG_COMPLETE        "완료되었습니다.")
(setq MSG_FILE_SAVED      "INP 파일이 생성되었습니다: ")
(setq MSG_PIPE_INFO       "파이프 정보: ")
(setq MSG_LENGTH          "길이: ")
(setq MSG_VERTICES        "정점 수: ")
(setq MSG_DATA_SORT       "데이터 정렬 중...")

;; 한글 출력을 위한 문자열 변환 함수
(defun convert-string (str)
  (acet-list-to-str 
    (acet-str-to-list str)
  )
)

;; 한글 메시지 출력 함수
(defun princ-kr (str)
  (princ str)
)

(defun C:GETINP ( / fname selected-ss junction-list pipeline-list file)
  ;; 선택 객체 생성
  (setq selected-ss (ssget))
  (if (null selected-ss)
      (progn
        (princ "\n선택된 객체가 없습니다.")
        (exit)
      )
  )
  
  ;; 선택된 객체들의 타입별 리스트 생성
  (setq junction-list '()
        pipeline-list '())
  
  (setq i 0)
  (repeat (sslength selected-ss)
    (setq ent (ssname selected-ss i))
    (setq ent-data (entget ent))
    (cond 
      ;; Junction 체크
      ((and (= "TEXT" (cdr (assoc 0 ent-data)))
            (= "L_Junction" (cdr (assoc 8 ent-data))))
       (setq junction-list (cons ent junction-list)))
      ;; Pipeline 체크
      ((and (= "LWPOLYLINE" (cdr (assoc 0 ent-data)))
            (= "L_pipeline" (cdr (assoc 8 ent-data))))
       (setq pipeline-list (cons ent pipeline-list)))
    )
    (setq i (1+ i))
  )
  
  ;; 리스트 유효성 확인
  (if (or (null junction-list) (null pipeline-list))
      (progn
        (princ "\nJunction과 Pipeline이 모두 선택되어야 합니다.")
        (exit)
      )
  )
  
  ;; 선택세트 생성
  (setq junction-ss (ssadd))
  (foreach ent junction-list
    (ssadd ent junction-ss))
  
  (setq pipeline-ss (ssadd))
  (foreach ent pipeline-list
    (ssadd ent pipeline-ss))
  
  ;; 파일 생성
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

  ;; [TIMES] 정보 추가
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
  
  ;; [OPTIONS] 정보 추가
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
  
  ;; 파일 닫기
  (close file)
  (princ "\n=========================================")
  (princ "\n          ⓒ2025 KUNHWA Corp.           ")
  (princ "\n            Created by TW              ")
  (princ "\n=========================================")
  (princ)
)

;; Junction과 Reservoir 정보를 파일에 기록하는 함수
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

;; Coordinates 정보를 파일에 기록하는 함수
(defun write-coordinates (file selected-ss / ss obj txt pt x y i)
  (setq ss selected-ss)
  (if ss
    (progn
      (write-line "[COORDINATES]" file)
      (write-line ";Node            X-Coord         Y-Coord" file)

      (setq i 0)
      (while (< i (sslength ss))
        (setq obj (ssname ss i))
        (setq obj-data (entget obj))
        ;; L_Junction 타입의 TEXT 객체들 중에서 추출
        (if (and (= (cdr (assoc 0 obj-data)) "TEXT")
                 (= (cdr (assoc 8 obj-data)) "L_Junction"))
          (progn
            (setq txt (cdr (assoc 1 obj-data)))    ; 텍스트 내용
            (setq pt (cdr (assoc 10 obj-data)))    ; 좌표
            (setq x (rtos (car pt) 2 12))          ; X좌표
            (setq y (rtos (cadr pt) 2 12))         ; Y좌표
            (write-line (strcat txt "               " x "         " y) file)
          )
        )
        (setq i (1+ i))
      )
      (write-line "" file)
    )
  )
)

;; 특정 좌표에서 텍스트 추출하는 함수
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

;; 특정 좌표에서 가장 가까운 텍스트 추출하는 함수
(defun get_text_near_point (pt layer / ss txt)
  (setq ss (ssget "C" 
                  (list (- (car pt) 1) (- (cadr pt) 1)) 
                  (list (+ (car pt) 1) (+ (cadr pt) 1)) 
                  (list '(0 . "TEXT") (cons 8 layer))))
  (if ss
    (setq txt (cdr (assoc 1 (entget (ssname ss 0))))) ; 첫 번째 텍스트 객체 반환
    (setq txt "N/A")) ; 텍스트가 없는 경우 "N/A" 반환
  txt
)

;; 특정 폴리라인 객체에서 정점들을 추출하는 함수
(defun get_polyline_vertices (pline / num-verts vertices i pt)
  (setq num-verts (vlax-curve-getendparam pline))
  (setq vertices '())
  (setq i 1) ; 0부터 num-verts까지 순환
  (while (< i num-verts)
    (setq pt (vlax-curve-getpointatparam pline i))
    (if (not (member pt vertices)) ; 중복 제거
      (setq vertices (cons pt vertices)))
    (setq i (1+ i)))
  vertices ; 중복 제거된 정점들의 리스트 반환
)

;; Pipes와 Vertices 정보를 파일에 기록하는 함수
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

        ;; ID 추출
        (setq id (get_text_near_point midpt "L_pipes"))
        (princ-kr (strcat MSG_PIPE_INFO id))

        ;; Node1 추출
        (setq startpt (vlax-curve-getstartpoint pline))
        (setq node1 (get_text_near_point startpt "L_Junction"))
        (princ-kr (strcat MSG_PIPE_INFO node1))

        ;; Node2 추출
        (setq endpt (vlax-curve-getendpoint pline))
        (setq node2 (get_text_near_point endpt "L_Junction"))
        (princ-kr (strcat MSG_PIPE_INFO node2))

        ;; Diameter 추출
        (setq dia (get_text_near_point midpt "L_diameter"))
        (princ-kr (strcat MSG_PIPE_INFO dia))

        ;; 데이터 리스트에 추가
        (setq data_list 
              (cons (list id node1 node2 (rtos len 2 2) dia "110" "0" "Open") data_list))

        ;; VERTICES 추출
        (setq vertices (get_polyline_vertices pline))
        (princ-kr (strcat MSG_VERTICES (itoa (length vertices))))
        (foreach vtx vertices
          (setq vertices_list (cons (list id 
                                        (rtos (car vtx) 2 12) 
                                        (rtos (cadr vtx) 2 12)) 
                                  vertices_list)))

        (setq i (1+ i))
      )

      ;; ID 정렬
      (setq data_list (vl-sort data_list '(lambda (a b) (< (car a) (car b)))))
      (setq vertices_list (vl-sort vertices_list '(lambda (a b) (< (car a) (car b)))))

      (princ-kr MSG_DATA_SORT)
      
      ;; [PIPES] 부분
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

      ;; [VERTICES] 부분
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
    (princ-kr "\n추출된 Pipeline 객체가 없습니다.")
  )
)
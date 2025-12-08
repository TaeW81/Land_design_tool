;;; 파이프라인 교차점 검사 리습 v0.1
;;; 작성일: 2024년
;;; 기능: 파이프라인 선들이 1m 내에 있지만 정확히 교차하지 않는 위치를 찾아서 표시

(defun c:pipe_check ()
  (setq *error* *error_handler*)
  (vl-load-com)
  (princ "\n=== 파이프라인 교차점 검사 시작 ===\n")
  
  ;; 사용자가 선 객체를 직접 선택하도록 안내
  (princ "\n검사할 선 객체들을 선택하세요 (전체 선택: ALL 입력)")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  
  ;; ALL 입력 시 전체 선택
  (if (and (null ss) (wcmatch (getstring "\n전체 선택하시겠습니까? (Y/N): ") "Y,y"))
    (setq ss (ssget "X" '((0 . "LINE,LWPOLYLINE,POLYLINE"))))
  )
  
  (if (null ss)
    (progn
      (princ "\n선 객체를 찾을 수 없습니다.")
      (exit)
    )
  )
  
  (setq line_count (sslength ss))
  (princ (strcat "\n총 " (itoa line_count) "개의 선 객체를 선택했습니다."))
  
  ;; 검사 결과를 저장할 리스트
  (setq error_points '())
  
  ;; 선택집합을 안전한 엔티티 리스트로 변환
  (setq ent_list '())
  (setq i 0)
  (repeat line_count
    (setq ent (ssname ss i))
    (if (and ent (entget ent))
      (setq ent_list (cons ent ent_list))
    )
    (setq i (1+ i))
  )
  
  ;; 각 선에 대해 검사
  (foreach ent ent_list
    (setq ent_data (entget ent))
    (setq ent_type (cdr (assoc 0 ent_data)))
    
    ;; 선의 끝점들 추출
    (setq endpoints (get_line_endpoints ent ent_type))
    
    ;; 끝점이 성공적으로 추출된 경우에만 검사
    (if endpoints
      (progn
        ;; 각 끝점에 대해 검사
        (foreach endpoint endpoints
          (setq nearby_lines (find_lines_within_distance endpoint 1.0 ent_list ent))
          
                     ;; 1m 내에 선이 있지만 끝점에서 만나지 않는 경우 확인
           (if (and nearby_lines (not (is_point_at_line_endpoint endpoint nearby_lines)))
             (progn
               (setq error_points (cons endpoint error_points))
               (princ (strcat "\n오류 발견: " (rtos (car endpoint) 2 3) ", " (rtos (cadr endpoint) 2 3)))
             )
           )
        )
      )
      (princ (strcat "\n엔티티 " (vl-princ-to-string ent) " 건너뜀"))
    )
  )
  
  ;; 오류 지점들을 캐드에 표시
  (if error_points
    (progn
      (princ (strcat "\n\n총 " (itoa (length error_points)) "개의 오류 지점을 발견했습니다."))
      (princ "\n오류 지점들을 캐드에 표시합니다...")
      (mark_error_points error_points)
    )
    (princ "\n\n모든 파이프라인이 정상적으로 교차하고 있습니다.")
  )
  
  (princ "\n=== 파이프라인 교차점 검사 완료 ===\n")
  (princ)
)

;;; 선의 끝점들을 추출하는 함수
(defun get_line_endpoints (ent ent_type)
  (if (and ent (entget ent))
    (progn
      (setq res (vl-catch-all-apply
                  (function
                    (lambda ()
                      (setq obj (vlax-ename->vla-object ent))
                      (list (vlax-curve-getStartPoint obj)
                            (vlax-curve-getEndPoint obj))
                    )
                  )
                ))
      (if (vl-catch-all-error-p res) 
        (progn
          (princ (strcat "\n엔티티 " (vl-princ-to-string ent) "의 끝점 추출 실패"))
          nil
        )
        res
      )
    )
    nil
  )
)

;;; 폴리라인의 끝점들을 추출하는 함수
(defun get_polyline_endpoints (ent)
  (setq ent_data (entget ent))
  (setq vertices '())
  
  (foreach item ent_data
    (if (= (car item) 10)
      (setq vertices (append vertices (list (cdr item))))
    )
  )
  
  (if (>= (length vertices) 2)
    (list (car vertices) (car (last vertices)))
    nil
  )
)

;;; 주어진 점에서 특정 거리 내에 있는 선들을 찾는 함수
(defun find_lines_within_distance (point tol ent_list ent-self)
  (setq nearby_lines '())
  
  (foreach ent ent_list
    ;; 자기 자신은 제외 (ename 비교는 eq 사용)
    (if (and ent-self (eq ent ent-self))
      (setq ent nil)
    )
    
    (if ent
      (progn
        (setq ent_data (entget ent))
        (setq ent_type (cdr (assoc 0 ent_data)))
        
        ;; 선이 주어진 점에서 거리 내에 있는지 확인
        (if (is_line_within_distance point tol ent ent_type)
          (setq nearby_lines (cons ent nearby_lines))
        )
      )
    )
  )
  
  nearby_lines
)

;;; 선이 주어진 점에서 특정 거리 내에 있는지 확인하는 함수
(defun is_line_within_distance (point tol ent ent_type)
  (<= (closest-distance-to-entity point ent) tol)
)

;;; 점에서 선까지의 최단 거리를 계산하는 함수
(defun distance_to_line (point line_start line_end)
  (setq dx (- (car line_end) (car line_start)))
  (setq dy (- (cadr line_end) (cadr line_start)))
  (setq line_length (sqrt (+ (* dx dx) (* dy dy))))
  
  (if (= line_length 0.0)
    (distance point line_start)
    (progn
      (setq t_param (/ (+ (* (- (car point) (car line_start)) dx)
                          (* (- (cadr point) (cadr line_start)) dy))
                       (* line_length line_length)))
      
      (if (<= t_param 0.0)
        (distance point line_start)
        (if (>= t_param 1.0)
          (distance point line_end)
          (progn
            (setq closest_pt (list (+ (car line_start) (* t_param dx))
                                   (+ (cadr line_start) (* t_param dy))))
            (distance point closest_pt)
          )
        )
      )
    )
  )
)

;;; 폴리라인이 주어진 점에서 특정 거리 내에 있는지 확인하는 함수
(defun is_polyline_within_distance (point tol ent)
  (setq ent_data (entget ent))
  (setq vertices '())
  
  (foreach item ent_data
    (if (= (car item) 10)
      (setq vertices (append vertices (list (cdr item))))
    )
  )
  
  ;; 각 선분에 대해 거리 확인
  (setq i 0)
  (setq within_distance nil)
  
  (repeat (1- (length vertices))
    (setq start_pt (nth i vertices))
    (setq end_pt (nth (1+ i) vertices))
    
    (if (<= (distance_to_line point start_pt end_pt) tol)
      (setq within_distance t)
    )
    
    (setq i (1+ i))
  )
  
  within_distance
)

;;; 점이 선의 끝점에서 만나는지 확인하는 함수 (중간에 닿으면 안 됨)
(defun is_point_at_line_endpoint (point lines)
  (setq at_endpoint nil)
  (foreach line lines
    (if (is_point_at_specific_line_endpoint point line)
      (setq at_endpoint t)
    )
  )
  at_endpoint
)

;;; 특정 점이 선의 끝점에 있는지 확인하는 함수
(defun is_point_at_specific_line_endpoint (point line)
  (if (and line (entget line))
    (progn
      (setq res (vl-catch-all-apply
                  (function
                    (lambda ()
                      (setq obj (vlax-ename->vla-object line))
                      (setq start_pt (vlax-curve-getStartPoint obj))
                      (setq end_pt (vlax-curve-getEndPoint obj))
                      
                      ;; 점이 시작점 또는 끝점과 매우 가까운지 확인 (0.001 이내)
                      (or (<= (distance point start_pt) 0.001)
                          (<= (distance point end_pt) 0.001))
                    )
                  )
                ))
      (if (vl-catch-all-error-p res) nil res)
    )
    nil
  )
)

;;; 주어진 점에서 엔티티까지의 최근접 거리
(defun closest-distance-to-entity (pt ent)
  (if (and ent (entget ent))
    (progn
      (setq res (vl-catch-all-apply
                  (function
                    (lambda ()
                      (setq obj (vlax-ename->vla-object ent))
                      (setq cp (vlax-curve-getClosestPointTo obj (ensure-3d pt)))
                      (distance pt cp)
                    )
                  )
                ))
      (if (vl-catch-all-error-p res) 1.0e99 res)
    )
    1.0e99
  )
)

;;; 2D/3D 점을 항상 3D 리스트로 보정
(defun ensure-3d (p)
  (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
)

;;; 점이 폴리라인 위에 있는지 확인하는 함수
(defun is_point_on_polyline (point ent)
  (setq ent_data (entget ent))
  (setq vertices '())
  
  (foreach item ent_data
    (if (= (car item) 10)
      (setq vertices (cons (cdr item) vertices))
    )
  )
  
  ;; 각 선분에 대해 확인
  (setq i 0)
  (setq on_line nil)
  
  (repeat (1- (length vertices))
    (setq start_pt (nth i vertices))
    (setq end_pt (nth (1+ i) vertices))
    
    (if (<= (distance_to_line point start_pt end_pt) 0.001)
      (setq on_line t)
    )
    
    (setq i (1+ i))
  )
  
  on_line
)

;;; 오류 지점들을 캐드에 표시하는 함수
(defun mark_error_points (error_points)
  (setq layer_name "PIPE_CHECK_ERRORS")
  
  ;; 레이어 생성 (없는 경우)
  (if (not (tblsearch "LAYER" layer_name))
    (command "._layer" "m" layer_name "c" "red" "" "")
  )
  
  ;; 현재 레이어 저장
  (setq current_layer (getvar "CLAYER"))
  
  ;; 오류 레이어로 변경
  (command "._layer" "s" layer_name "")
  
  ;; 각 오류 지점에 원만 표시 (크기: 1.0)
  (foreach point error_points
    ;; 원 그리기
    (command "._circle" point 1.0)
  )
  
  ;; 원래 레이어로 복원
  (command "._layer" "s" current_layer "")
  
  (princ (strcat "\n" (itoa (length error_points)) "개의 오류 지점을 'PIPE_CHECK_ERRORS' 레이어에 표시했습니다."))
)

;;; 에러 핸들러
(defun *error_handler* (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
    (princ (strcat "\n오류 발생: " msg))
  )
  (princ)
)

;;; 리습 로드 메시지
(princ "\n파이프라인 교차점 검사 리습이 로드되었습니다.")
(princ "\n사용법: pipe_check")
(princ)

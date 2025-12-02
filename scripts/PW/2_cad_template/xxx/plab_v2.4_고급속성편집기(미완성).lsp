(defun c:PLAB (/ ss ht i en obj startPt endPt ptDict ptKey pt z angle midpt laydef ent totalLength)

  (vl-load-com)

  ;; 좌표 고유키 변환 함수
  (defun pt->key (pt)
    (vl-princ-to-string (mapcar (function (lambda (n) (rtos n 2 6))) pt)))

  ;; 각도 계산 함수
  (defun get-angle (p1 p2)
    (atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1))))

  ;; 텍스트 생성 함수
  (defun make-text (txt pt lay rot justH justV)
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 pt)
        (cons 11 pt)
        (cons 40 ht)
        (cons 1 txt)
        (cons 8 lay)
        (cons 7 "Standard")
        (cons 50 rot)
        (cons 72 justH)
        (cons 73 justV)
        (cons 41 1.0)
      )))

  ;; 고급속성편집기 함수 - 객체 중간의 L_Pipes와 L_diameter 텍스트를 한꺼번에 편집
  (defun c:PLAB_EDIT (/ ss i en obj startPt endPt midpt pipesText diaText pipesVal diaVal)
    (vl-load-com)
    
    (prompt "\n=== PLAB 고급속성편집기 ===")
    (prompt "\n편집할 파이프라인 객체를 선택하세요...")
    
    (if (setq ss (ssget '((0 . "LWPOLYLINE,LINE"))))
      (progn
        (setq i 0)
        (while (< i (sslength ss))
          (setq en (ssname ss i))
          (setq obj (vlax-ename->vla-object en))
          
          ;; 시점과 종점
          (setq startPt (vlax-curve-getStartPoint obj))
          (setq endPt (vlax-curve-getEndPoint obj))
          
          ;; 중간점 계산
          (setq totalLength (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
          (setq midpt (vlax-curve-getPointAtDist obj (/ totalLength 2.0)))
          
          ;; 중간점 근처의 L_Pipes와 L_diameter 텍스트 찾기
          (setq pipesText (find-text-near-point midpt "L_Pipes"))
          (setq diaText (find-text-near-point midpt "L_diameter"))
          
          ;; 현재 값 표시
          (prompt (strcat "\n\n객체 " (itoa (1+ i)) " - 중간점: " (vl-princ-to-string midpt)))
          (if pipesText
            (prompt (strcat "\n현재 Pipes 값: " (cdr (assoc 1 (entget pipesText)))))
            (prompt "\n현재 Pipes 값: 없음"))
          (if diaText
            (prompt (strcat "\n현재 Diameter 값: " (cdr (assoc 1 (entget diaText)))))
            (prompt "\n현재 Diameter 값: 없음"))
          
          ;; 새 값 입력
          (setq pipesVal (getstring "\n새로운 Pipes 값을 입력하세요 (엔터로 건너뛰기): "))
          (if (= pipesVal "") (setq pipesVal nil))
          
          (setq diaVal (getstring "\n새로운 Diameter 값을 입력하세요 (엔터로 건너뛰기): "))
          (if (= diaVal "") (setq diaVal nil))
          
          ;; 텍스트 업데이트
          (if (and pipesText pipesVal)
            (update-text pipesText pipesVal))
          (if (and diaText diaVal)
            (update-text diaText diaVal))
          
          ;; 텍스트가 없고 새 값이 있으면 생성
          (if (and (not pipesText) pipesVal)
            (progn
              (setq angle (get-angle startPt endPt))
              (make-text pipesVal midpt "L_Pipes" angle 0 0)))
          (if (and (not diaText) diaVal)
            (progn
              (setq angle (get-angle startPt endPt))
              (make-text diaVal midpt "L_diameter" angle 0 3)))
          
          (setq i (1+ i)))
        (prompt "\n✅ 편집이 완료되었습니다."))
      (prompt "\n❌ 편집할 객체가 선택되지 않았습니다."))
    (princ))

  ;; 중간점 근처의 특정 레이어 텍스트 찾기 함수
  (defun find-text-near-point (pt lay / ss i en ent entPt dist minDist result)
    (setq ss (ssget "X" (list (cons 0 "TEXT") (cons 8 lay))))
    (if ss
      (progn
        (setq minDist 999999.0)
        (setq i 0)
        (while (< i (sslength ss))
          (setq en (ssname ss i))
          (setq ent (entget en))
          (setq entPt (cdr (assoc 10 ent)))
          (setq dist (distance pt entPt))
          (if (< dist minDist)
            (progn
              (setq minDist dist)
              (setq result en)))
          (setq i (1+ i)))
        ;; 10 단위 이내에 있으면 해당 텍스트 반환
        (if (< minDist 10.0) result nil))
      nil))

  ;; 텍스트 내용 업데이트 함수
  (defun update-text (en newText / ent)
    (setq ent (entget en))
    (setq ent (subst (cons 1 newText) (assoc 1 ent) ent))
    (entmod ent)
    (entupd en))

  ;; 텍스트 높이 입력
  (setq ht (getreal "\n텍스트 높이를 입력하세요 <2.5>: "))
  (if (not ht) (setq ht 2.5))

  ;; 레이어 생성 또는 수정
  (foreach lay '(("L_Junction" . 1) ("L_FH" . 5) ("L_Pipes" . 2) ("L_diameter" . 3) ("L_pipeline" . 4)) ; "L_Dia" → "L_diameter"
    (if (not (tblsearch "LAYER" (car lay)))
      (entmake (list (cons 0 "LAYER") (cons 2 (car lay)) (cons 62 (cdr lay))))
      (progn
        (setq laydef (tblobjname "LAYER" (car lay)))
        (setq ent (entget laydef))
        (if (assoc 62 ent)
          (setq ent (subst (cons 62 (cdr lay)) (assoc 62 ent) ent))
          (setq ent (append ent (list (cons 62 (cdr lay))))))
        (entmod ent)
        (entupd laydef))))

  ;; 폴리선 및 선 선택
  (if (setq ss (ssget '((0 . "LWPOLYLINE,LINE"))))
    (progn
      (setq ptDict '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i))
        (setq obj (vlax-ename->vla-object en))

        ;; 선택한 객체의 레이어를 L_pipeline으로 변경
        (vla-put-Layer obj "L_pipeline")

        ;; 시점과 종점
        (setq startPt (vlax-curve-getStartPoint obj))
        (setq endPt   (vlax-curve-getEndPoint obj))

        ;; 중복 방지 및 텍스트
        (foreach pt (list startPt endPt)
          (setq z (if (= (length pt) 3) (nth 2 pt) 0.0))
          (setq pt (list (car pt) (cadr pt) z))
          (setq ptKey (pt->key pt))
          (if (not (assoc ptKey ptDict))
            (progn
              (setq ptDict (cons (cons ptKey pt) ptDict))
              (make-text "Junction" pt "L_Junction" 0.0 0 0)
              (make-text "FH" pt "L_FH" 0.0 0 3))))

        ;; 중간점 계산: 선 길이의 절반 위치
        (setq totalLength (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
        (setq midpt (vlax-curve-getPointAtDist obj (/ totalLength 2.0)))

        ;; 각도 계산
        (setq angle (get-angle startPt endPt))

        ;; Dia 텍스트 (왼쪽 위)
        (make-text "Dia" midpt "L_diameter" angle 0 3) ; "L_Dia" → "L_diameter"

        ;; Line 텍스트 (중앙)
        (make-text "Line" midpt "L_Pipes" angle 0 0)

        (setq i (1+ i))))
    (prompt "\n❌ 폴리선 또는 선이 선택되지 않았습니다."))

  (princ))

;; ========================================
;; 사용법 안내
;; ========================================
;; 
;; 1. PLAB 명령어: 파이프라인 객체에 기본 텍스트 생성
;;    - 폴리선 또는 선을 선택하면 자동으로 Junction, FH, Pipes, Diameter 텍스트 생성
;; 
;; 2. PLAB_EDIT 명령어: 고급속성편집기
;;    - 기존 파이프라인 객체의 중간점에 있는 L_Pipes와 L_diameter 텍스트를 한꺼번에 편집
;;    - 각 객체별로 현재 값을 표시하고 새 값을 입력받음
;;    - 엔터를 누르면 해당 값은 건너뛰기
;;    - 텍스트가 없으면 새로 생성
;; 
;; 레이어 정보:
;; - L_Junction: Junction 텍스트 (색상: 1)
;; - L_FH: FH 텍스트 (색상: 5)  
;; - L_Pipes: Pipes 텍스트 (색상: 2)
;; - L_diameter: Diameter 텍스트 (색상: 3)
;; - L_pipeline: 파이프라인 객체 (색상: 4)
;; ========================================

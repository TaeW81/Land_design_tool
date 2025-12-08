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

  ;; 배수지 정션을 선택하라는 메시지 표시
  (princ "\n")
  (princ "\n==========================================")
  (princ "\n   🚰 배수지 정션을 선택하세요! 🚰")
  (princ "\n==========================================")
  (princ "\n")
  
  ;; 사용자가 직접 하나의 junction 텍스트를 선택
  (setq ss (ssget '((0 . "TEXT") (1 . "Junction") (8 . "L_Junction"))))
  (if ss
    (progn
      (princ "\n선택된 Junction을 Reservoir로 변경합니다...")
      ;; 선택된 첫 번째 junction만 변경
      (setq en (ssname ss 0))
      (setq ent (entget en))
      ;; 텍스트 내용을 "Reservoir"로 변경
      (setq ent (subst (cons 1 "Reservoir") (assoc 1 ent) ent))
      (entmod ent)
      (princ "\n✅ 선택된 Junction이 Reservoir로 변경되었습니다."))
    (princ "\nJunction 텍스트가 선택되지 않았습니다."))

  (princ))

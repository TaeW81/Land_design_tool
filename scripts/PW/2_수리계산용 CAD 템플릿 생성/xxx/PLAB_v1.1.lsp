(defun c:PLAB (/ ss ht i en obj startPt endPt ptDict ptKey pt z param midpt angle laydef ent)

  (vl-load-com)

  ;; 좌표 고유키 변환 함수
  (defun pt->key (pt)
    (vl-princ-to-string (mapcar (function (lambda (n) (rtos n 2 6))) pt)))

  ;; 각도 계산 함수 (벡터 이용)
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

  ;; 텍스트 높이 설정
  (setq ht (getreal "\n텍스트 높이를 입력하세요 <2.5>: "))
  (if (not ht) (setq ht 2.5))

  ;; 레이어 생성 및 색상 지정 (존재하면 수정)
  (foreach lay '(("L_Junction" . 1) ("L_FH" . 5) ("L_Pipes" . 2) ("L_Dia" . 3))
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

  ;; 폴리선 선택
  (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
    (progn
      (setq ptDict '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq en (ssname ss i))
        (setq obj (vlax-ename->vla-object en))

        ;; 시점과 종점 추출
        (setq startPt (vlax-curve-getStartPoint obj))
        (setq endPt   (vlax-curve-getEndPoint obj))

        ;; 각도 계산 (Line과 Dia에 사용)
        (setq angle (get-angle startPt endPt))

        ;; 중복 방지 후 텍스트 생성
        (foreach pt (list startPt endPt)
          (setq z (if (= (length pt) 3) (nth 2 pt) 0.0))
          (setq pt (list (car pt) (cadr pt) z))
          (setq ptKey (pt->key pt))
          (if (not (assoc ptKey ptDict))
            (progn
              (setq ptDict (cons (cons ptKey pt) ptDict))
              ;; Junction
              (make-text "Junction" pt "L_Junction" 0.0 0 0)
              ;; FH
              (make-text "FH" pt "L_FH" 0.0 0 3))))

        ;; 중간 지점 계산
        (setq param (/ (vlax-curve-getEndParam obj) 2.0))
        (setq midpt (vlax-curve-getPointAtParam obj param))

        ;; Dia (정렬: 왼쪽 위)
        (make-text "Dia" midpt "L_diameter" angle 0 3)

        ;; Line 텍스트이지만 L_Pipes 레이어에 배치
        (make-text "Line" midpt "L_Pipes" angle 0 0)

        (setq i (1+ i))))
    (prompt "\n❌ 폴리선이 선택되지 않았습니다."))
  (princ))

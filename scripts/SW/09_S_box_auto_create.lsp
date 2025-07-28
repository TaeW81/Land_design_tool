(defun c:p_boxauto (/ ent baseObjs vlaEnt minPt maxPt width height
                 addGap dir count dx dy i)

  (vl-load-com)
  (prompt "\n[MPA - 종단면도 도각 자동 배치]")

  ;; 1. 기준 도각 단일 선택
  (prompt "\n기준이 될 도각 객체를 선택하세요: ")
  (setq ent (car (entsel)))
  (if (not ent)
    (progn (prompt "\n[오류] 선택이 취소되었습니다.") (exit))
  )

  ;; 2. 도각과 함께 복사할 추가 객체 선택
  (prompt "\n도각과 함께 복사할 객체들을 선택하세요 (드래그 또는 클릭, Enter 시 도각만 복사): ")
  (setq baseObjs (ssget))
  (if (not baseObjs)
    (progn 
      (prompt "\n[안내] 선택된 객체가 없습니다. 도각만 복사합니다.")
      (setq baseObjs (ssadd))  ; 빈 선택집합 생성
    )
  )

  ;; 도각 객체도 복사 집합에 포함
  (setq baseObjs (ssadd ent baseObjs))

  ;; 3. 선택된 도각 객체의 바운딩 박스 계산
  (setq vlaEnt (vlax-ename->vla-object ent))
  (vla-getboundingbox vlaEnt 'minPt 'maxPt)
  (setq minPt (vlax-safearray->list minPt))
  (setq maxPt (vlax-safearray->list maxPt))

  (setq width  (- (car maxPt) (car minPt)))
  (setq height (- (cadr maxPt) (cadr minPt)))

  (prompt (strcat "\n도각 가로 길이: " (rtos width 2 2)))
  (prompt (strcat ", 세로 길이: " (rtos height 2 2)))

  ;; 4. 추가 간격 입력
  (initget 4)
  (setq addGap (getreal "\n도각 간격을 추가로 입력하세요 (0 입력 가능): "))
  (if (or (null addGap) (< addGap 0))
    (progn (prompt "\n[오류] 0 이상의 숫자를 입력하세요.") (exit))
  )

  ;; 5. 방향 선택
  (prompt "\n복사할 방향을 선택하세요:")
  (prompt "\n 1 = 오른쪽 (X+), 2 = 왼쪽 (X-), 3 = 위쪽 (Y+), 4 = 아래쪽 (Y-)")
  (initget 1 "1 2 3 4")
  (setq dir (getkword "\n방향 [1/2/3/4]: "))

  ;; 6. 개수 입력
  (initget 6)
  (setq count (getint "\n복사할 도각 개수를 입력하세요: "))
  (if (null count)
    (progn (prompt "\n[오류] 개수 입력이 취소되었습니다.") (exit))
  )

  ;; 7. 방향에 따른 이동 거리 설정
  (cond
    ((= dir "1") (setq dx (+ width addGap) dy 0.0))  ; 오른쪽
    ((= dir "2") (setq dx (- (+ width addGap)) dy 0.0)) ; 왼쪽
    ((= dir "3") (setq dx 0.0 dy (+ height addGap))) ; 위쪽
    ((= dir "4") (setq dx 0.0 dy (- (+ height addGap)))) ; 아래쪽
  )

  ;; 8. 반복 복사 실행
  (setq i 1)
  (while (<= i count)
    (command "_copy" baseObjs "" '(0 0 0) (list (* dx i) (* dy i) 0))
    (setq i (1+ i))
  )

  (prompt "\n[완료] 도각과 추가 객체들이 반복 생성되었습니다.")
  (princ)
)

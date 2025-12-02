(defun c:p_boxauto (/ ent baseObjs vlaEnt minPt maxPt width height
                 addGap rightCount downCount dx dy row col)

  (vl-load-com)
  (prompt "\n[MPA - 종단면도 도각 자동 배치]")

  ;; 기준 도각 단일 선택
  (prompt "\n기준이 될 도각 객체를 선택하세요: ")
  (setq ent (car (entsel)))
  (if (not ent)
    (progn (prompt "\n[오류] 선택이 취소되었습니다.") (exit))
  )

  ;; 도각과 함께 복사할 추가 객체 선택
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

  ;; 도각 바운딩 박스 계산
  (setq vlaEnt (vlax-ename->vla-object ent))
  (vla-getboundingbox vlaEnt 'minPt 'maxPt)
  (setq minPt (vlax-safearray->list minPt))
  (setq maxPt (vlax-safearray->list maxPt))
  (setq width  (- (car maxPt) (car minPt)))
  (setq height (- (cadr maxPt) (cadr minPt)))

  ;; 간격 입력
  (initget 4)
  (setq addGap (getreal "\n도각 간격을 추가로 입력하세요 (0 가능): "))
  (if (or (null addGap) (< addGap 0))
    (progn (prompt "\n[오류] 0 이상의 숫자를 입력하세요.") (exit))
  )

  ;; 오른쪽 개수 입력
  (initget 6)
  (setq rightCount (getint "\n오른쪽으로 복사할 개수를 입력하세요 (0 가능): "))
  (if (null rightCount)
    (progn (prompt "\n[오류] 입력이 취소되었습니다.") (exit))
  )

  ;; 아래쪽 개수 입력
  (initget 6)
  (setq downCount (getint "\n아래쪽으로 복사할 개수를 입력하세요 (0 가능): "))
  (if (null downCount)
    (progn (prompt "\n[오류] 입력이 취소되었습니다.") (exit))
  )

  ;; 반복 복사 실행
  (setq col 0)
  (while (<= col rightCount)
    (setq row 0)
    (while (<= row downCount)
      (if (or (/= col 0) (/= row 0)) ; 기준 도각은 제외
        (command "_copy" baseObjs "" '(0 0 0) (list (* (+ width addGap) col) (* (- (+ height addGap)) row) 0))
      )
      (setq row (1+ row))
    )
    (setq col (1+ col))
  )

  (prompt "\n[완료] 도각과 추가 객체들이 반복 생성되었습니다.")
  (princ)
)

(defun c:SWAR ( / drawArrow doc ms ss i ent ep param dirVec successCount)

  ;; 화살표 생성 함수 (종점 기준 방향)
  (defun drawArrow (tipPt dirVec / ang width p1 p2 basePt newTip)
    (if (and tipPt dirVec)
      (progn
        (setq width 5.0)
        (setq ang (angle '(0 0 0) dirVec))
        (setq newTip (polar tipPt (+ ang pi) 0.5))   ; 꼭짓점 약간 뒤로
        (setq basePt (polar newTip (+ ang pi) 7.5))  ; 밑변 중심
        (setq p1 (polar basePt (+ ang (/ pi 2)) (/ width 2)))
        (setq p2 (polar basePt (- ang (/ pi 2)) (/ width 2)))
        (entmakex
          (list
            '(0 . "SOLID")
            (cons 8 "01.우수 화살표")
            (cons 62 1)
            (cons 10 p1)
            (cons 11 p2)
            (cons 12 newTip)
            (cons 13 newTip)
          )
        )
      )
    )
  )

  ;; 레이어 보장
  (if (not (tblsearch "LAYER" "01.우수 화살표"))
    (entmakex
      (list '(0 . "LAYER") (cons 2 "01.우수 화살표") (cons 62 1))
    )
  )

  ;; 사용자 선택
  (prompt "\n화살표를 생성할 폴리라인을 선택하세요:")
  (setq ss (ssget '((0 . "LWPOLYLINE")))) ; 🎯 레이어 필터 제거됨

  (setq successCount 0)

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq ep (vlax-curve-getEndPoint ent))
        (setq param (vlax-curve-getEndParam ent))
        (setq dirVec (vlax-curve-getFirstDeriv ent param))
        (if (and ep dirVec)
          (progn
            (drawArrow ep dirVec)
            (setq successCount (1+ successCount))
          )
        )
        (setq i (1+ i))
      )
      (prompt (strcat "\n[✔] 생성된 화살표 수: " (itoa successCount)))
    )
    (prompt "\n[!] 선택된 객체가 없습니다.")
  )

  (princ)
)

(defun c:ro180 ( / ent blk inspt)
  (prompt "\n블록을 선택하세요: ")
  (setq ent (entsel))
  (if ent
    (progn
      (setq blk (car ent))  ; 선택한 블록 참조
      (setq inspt (cdr (assoc 10 (entget blk))))  ; 블록의 삽입점 좌표 추출

      ;; 회전 명령 실행 (삽입점 기준 180도)
      (command "_.rotate" blk "" inspt "180")
    )
    (prompt "\n블록을 선택하지 않았습니다.")
  )
  (princ)
)

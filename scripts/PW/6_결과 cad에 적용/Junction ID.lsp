(defun c:EPANET_ID (/ fn file line data nodeid x y count)
  (setq fn (getfiled "Select CSV File" "" "csv" 4))
  (if fn
    (progn
      (setq file (open fn "r"))
      (setq count 0)
      (read-line file) ; 첫 줄 (헤더) 스킵
      (while (setq line (read-line file))
        (if (> (strlen line) 0) ; 빈 줄이 아닌 경우만 처리
          (progn
            ;; CSV 한 줄을 쉼표 기준으로 분리
            (setq data (split-csv-line line))
            
            ;; NodeID, X, Y 컬럼 추출 (최소 3개 컬럼 필요)
            (if (and data (>= (length data) 3))
              (progn
                (setq nodeid (nth 0 data))
                (setq x (atof (nth 1 data)))
                (setq y (atof (nth 2 data)))
                
                ;; 유효한 좌표인지 확인
                (if (and (numberp x) (numberp y) (not (equal x 0.0 1e-6)) (not (equal y 0.0 1e-6)))
                  (progn
                    ;; 기존 블록이 있으면 삭제
                    (if (tblsearch "BLOCK" (strcat "NODE_" nodeid))
                      (command "PURGE" "B" (strcat "NODE_" nodeid) "Y" "Y")
                    )
                    
                    ;; 원 그리기 (반지름 2.5)
                    (command "CIRCLE" (list x y) 2.5)
                    ;; 원에 솔리드 해치 추가 (색상 254)
                    (command "HATCH" "SOLID" (list x y) "")
                    ;; 해치 색상 변경
                    (setq hatchent (entlast))
                    (if hatchent
                      (progn
                        (setq hatchdata (entget hatchent))
                        (setq hatchdata (subst (cons 62 254) (assoc 62 hatchdata) hatchdata))
                        (entmod hatchdata)
                      )
                    )
                    ;; 원과 해치를 블록으로 생성
                    (command "BLOCK" (strcat "NODE_" nodeid) (list x y) "W" (list (- x 3) (- y 3)) (list (+ x 3) (+ y 3)) "")
                    ;; 블록 삽입
                    (command "INSERT" (strcat "NODE_" nodeid) (list x y) 1 1 0)
                    ;; CAD에 TEXT 삽입 (폰트 크기 2.5, 폭비율 0.7, 중심 정렬, 파란색)
                    (command "TEXT" "J" "MC" (list x y) 2.5 0 nodeid)
                    ;; 폭비율과 색상을 entmod로 설정
                    (setq lastent (entlast))
                    (if lastent
                      (progn
                        (setq entdata (entget lastent))
                        (setq entdata (subst (cons 41 0.7) (assoc 41 entdata) entdata))
                        (if (assoc 62 entdata)
                          (setq entdata (subst (cons 62 5) (assoc 62 entdata) entdata))
                          (setq entdata (append entdata (list (cons 62 5))))
                        )
                        (entmod entdata)
                      )
                    )
                    (setq count (1+ count))
                    (princ (strcat "\nID: " nodeid " at (" (rtos x 2 2) ", " (rtos y 2 2) ")"))
                  )
                )
              )
            )
          )
        )
      )
      (close file)
      (princ (strcat "\n총 " (itoa count) "개의 ID가 CAD에 삽입되었습니다."))
    )
  )
  (princ)
)

;; CSV 라인을 쉼표로 분리하는 함수
(defun split-csv-line (line / result current i char)
  (setq result '())
  (setq current "")
  (setq i 1)
  (while (<= i (strlen line))
    (setq char (substr line i 1))
    (if (= char ",")
      (progn
        (setq result (append result (list current)))
        (setq current "")
      )
      (setq current (strcat current char))
    )
    (setq i (1+ i))
  )
  (setq result (append result (list current)))
  result
)

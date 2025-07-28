(defun c:p_mhinsert ( / *error* getDiameter spacingList getSpacing ss i ent txt val spacing radius len ptList pos existList pt dia divCount stepDist)

  (defun *error* (msg)
    (princ (strcat "\n오류: " msg))
    (princ)
  )

  (defun getDiameter (txt)
    (if (setq val (vl-string-translate "D=mmD" "" txt))
      (atoi val)
    )
  )

  (defun spacingList ()
    (list
      (list 0 600 75)
      (list 601 1000 100)
      (list 1001 1500 150)
      (list 1501 9999 200)
    )
  )

  (defun getSpacing (dia / result)
    (foreach r (spacingList)
      (if (and (>= dia (car r)) (<= dia (cadr r)))
        (setq result (last r))
      )
    )
    result
  )

  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (setq i (1+ i))

        ;; 주변 텍스트 수집
        (setq txt nil)
        (setq alltexts (ssget "_X" '((0 . "TEXT"))))
        (if alltexts
          (progn
            (setq idx 0)
            (while (< idx (sslength alltexts))
              (setq t (ssname alltexts idx))
              (setq content (strcase (cdr (assoc 1 (entget t)))))
              (if (wcmatch content "D###,D####,D###MM,D####MM")
                (setq txt t)
              )
              (setq idx (1+ idx))
            )
          )
        )

        (if txt
          (progn
            (setq val (cdr (assoc 1 (entget txt))))
            (setq dia (getDiameter val))
            (setq spacing (getSpacing dia))
            (setq radius 2.5)
            (setq len (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent)))

            ;; 포인트 리스트 초기화
            (setq ptList '())

            ;; 전체 길이가 spacing 이하면 시종점만
            (if (<= len spacing)
              (progn
                (setq ptList
                  (list
                    (vlax-curve-getPointAtParam ent 0)
                    (vlax-curve-getPointAtParam ent (vlax-curve-getEndParam ent))
                  )
                )
              )
              ;; spacing 보다 크면 균등 분할
              (progn
                (setq divCount (fix (/ len spacing)))
                (if (< divCount 1) (setq divCount 1)) ; 안정성 보장
                (setq stepDist (/ len divCount))
                (setq pos 0)
                (while (<= pos len)
                  (setq pt (vlax-curve-getPointAtDist ent pos))
                  (setq ptList (cons pt ptList))
                  (setq pos (+ pos stepDist))
                )
              )
            )

            ;; 기존 원 좌표 수집
            (setq existList '())
            (setq allcircles (ssget "_X" '((0 . "CIRCLE"))))
            (if allcircles
              (progn
                (setq j 0)
                (while (< j (sslength allcircles))
                  (setq c (ssname allcircles j))
                  (setq existList (cons (cdr (assoc 10 (entget c))) existList))
                  (setq j (1+ j))
                )
              )
            )

            ;; 중복 없을 때만 삽입
            (foreach pt (reverse ptList)
              (if (not (member pt existList))
                (entmake
                  (list
                    '(0 . "CIRCLE")
                    (cons 10 pt)
                    (cons 40 radius)
                  )
                )
              )
            )

            (princ (strcat "\n[CALL] dia=" (itoa dia) ", len=" (rtos len 2 1) ", spacing=" (rtos spacing 2 1)))
          )
          (prompt "\n[ERROR] 관경 텍스트 인식 실패")
        )
      )
    )
    (prompt "\n폴리선을 선택하세요.")
  )
  (princ)
)

(defun c:p_flowdir ( / getFHValue getFHOnCurve drawArrow doc ms ss txts curve fhData ent obj)

  ;; 텍스트에서 FH 값 추출
  (defun getFHValue (text / pos)
    (if (setq pos (vl-string-search "FH:" text))
      (atof (substr text (+ pos 4)))
    )
  )

  ;; 중심선 위에 있는 FH들을 param 기준 정렬하여 수집
  (defun getFHOnCurve (curve txts / out txt val pt projPt param dist)
    (setq out '())
    (foreach txt txts
      (setq val (getFHValue (cdr (assoc 1 (entget txt)))))
      (setq pt  (cdr (assoc 10 (entget txt))))
      (if (and val pt)
        (progn
          (setq projPt (vlax-curve-getClosestPointTo curve pt))
          (setq dist (distance pt projPt))
          (if (< dist 10.0)
            (progn
              (setq param (vlax-curve-getParamAtPoint curve projPt))
              (if (numberp param)
                (setq out (cons (list val projPt param) out))
              )
            )
          )
        )
      )
    )
    (vl-sort out '(lambda (a b) (< (nth 2 a) (nth 2 b))))
  )

  ;; 삼각형 화살표 생성 (reverse: 방향 반전 여부)
  (defun drawArrow (curve param1 param2 reverse / midParam midPt tanVec ang side p1 p2 p3 tip)
    (setq midParam (/ (+ param1 param2) 2.0))
    (setq midPt (vlax-curve-getPointAtParam curve midParam))
    (setq tanVec (mapcar '- (vlax-curve-getPointAtParam curve (+ midParam 0.01)) midPt))
    (if reverse (setq tanVec (mapcar '- '(0 0 0) tanVec))) ; 방향 반전

    (if (and midPt tanVec)
      (progn
        (setq side 5.0) ; 반폭
        (setq ang (angle '(0 0 0) tanVec))
        (setq p1 midPt)
        (setq p2 (polar p1 (+ ang (/ pi 2)) side))
        (setq p3 (polar p1 (- ang (/ pi 2)) side))
        (setq tip (polar p1 ang 20.0)) ; 길이 20
        (entmakex
          (list
            '(0 . "SOLID")
            (cons 8 "도로흐름표시")
            (cons 10 p2)
            (cons 11 p3)
            (cons 12 tip)
            (cons 13 tip)
          )
        )
      )
    )
  )

  ;; 중심선 선택
  (setq ss (ssget '((8 . "도로중심선") (0 . "LWPOLYLINE"))))
  (if (not ss)
    (progn (prompt "\n중심선을 선택하지 않았습니다.") (return))
  )

  ;; 도로체인계획고 텍스트 수집
  (setq txts '())
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))
  (vlax-for obj ms
    (if (and
          (member (vla-get-ObjectName obj) '("AcDbText" "AcDbMText"))
          (= (vla-get-Layer obj) "도로체인계획고")
        )
      (setq txts (cons (vlax-vla-object->ename obj) txts))
    )
  )

  ;; 중심선마다 처리
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq curve (vlax-ename->vla-object ent))
    (setq fhData (getFHOnCurve curve txts))

    (if (> (length fhData) 1)
      (progn
        (setq j 0)
        (while (< j (- (length fhData) 1))
          (setq fh1 (nth j fhData))
          (setq fh2 (nth (+ j 1) fhData))
          (setq val1 (nth 0 fh1))
          (setq param1 (nth 2 fh1))
          (setq val2 (nth 0 fh2))
          (setq param2 (nth 2 fh2))

          ;; 높은 고도 → 낮은 고도 방향으로 화살표 생성
          (if (/= val1 val2)
            (progn
              (cond
                ((> val1 val2)
                 (if (< param1 param2)
                   (drawArrow curve param1 param2 nil) ; 방향 그대로
                   (drawArrow curve param2 param1 t)   ; 방향 반전
                 )
                )
                ((> val2 val1)
                 (if (< param2 param1)
                   (drawArrow curve param2 param1 nil)
                   (drawArrow curve param1 param2 t)
                 )
                )
              )
            )
          )
          (setq j (1+ j))
        )
      )
    )
    (setq i (1+ i))
  )

  (prompt "\nFH 기준 화살표 (고도 기준 정확히 반영) 생성 완료.")
  (princ)
)

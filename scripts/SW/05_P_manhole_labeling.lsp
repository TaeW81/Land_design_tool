;; ------------------------------------------------------------------------------
;; 📌 AFT – 맨홀 설명선 자동 생성 리습
;;
;; 이 리습은 다음 과정을 자동화합니다:
;; 1. 사용자가 드래그로 CIRCLE과 LINE을 선택합니다.
;; 2. 텍스트 높이를 직접 입력합니다 (기본값: 3.0).
;; 3. 원 중심에서 선을 따라 설명선을 그립니다.
;; 4. 설명선 끝에서 만나는 선 근처의 텍스트를 찾아 이름과 고도로 분리합니다.
;; 5. 텍스트 길이에 따라 설명선 길이를 자동 조정합니다.
;;    ⮕ 텍스트 높이가 3 이상: 해당 높이만큼 설명선 길이 감산
;;    ⮕ 텍스트 높이가 3 미만: 고정값 4만큼 감산
;;
;; 레이어: "맨홀설명선" (자동 생성)
;;
;; 📎 명령어 입력: AFT
;; ------------------------------------------------------------------------------

(defun getTextParts (txt / pos name elev)
  (if (and (setq pos (vl-string-search "(" txt)) (> pos 0))
    (progn
      (setq name (vl-string-trim " " (substr txt 1 pos)))
      (setq elev (vl-string-trim " )" (substr txt (+ pos 2))))
      (list name elev)
    )
    (list txt "")
  )
)

(defun drawLeaderLine (center name elev textHeight / p1 p2 namePt elevPt hLen lineEnt entData newEntData
                                nameLen elevLen maxLen estimatedTextLen spacing charWidthFactor widthFactor buffer extraSpace)

  ;; 설정값
  (setq widthFactor 0.8)         ; 문자폭 비율
  (setq charWidthFactor 0.91)    ; 폰트 기준 보정값
  (setq buffer 4)                ; 여유 거리
  (setq spacing 0.5)             ; 위/아래 텍스트 간격

  (setq p1 (polar center (* pi 0.25) 6))

  ;; 최대 텍스트 길이 예측 후 설명선 길이 결정
  (setq nameLen (strlen name))
  (setq elevLen (strlen elev))
  (setq maxLen (max nameLen elevLen))
  (setq estimatedTextLen (* maxLen textHeight widthFactor charWidthFactor))
  (setq hLen (+ estimatedTextLen buffer))

  ;; 텍스트 높이에 따라 설명선 길이에서 감산
  (if (< textHeight 3)
    (setq hLen (- hLen 4))
    (setq hLen (- hLen textHeight))
  )

  (setq p2 (polar p1 0 hLen))

  ;; 텍스트 위치 계산
  (setq namePt (polar p1 (* pi 0.5) spacing))
  (setq elevPt (polar p1 (* pi -0.5) (+ textHeight spacing)))

  ;; 설명선 생성
  (setq lineEnt
    (entmakex
      (append
        (list
          '(0 . "LWPOLYLINE")
          (cons 100 "AcDbEntity")
          (cons 8 "맨홀설명선")
          (cons 62 4)
          (cons 100 "AcDbPolyline")
          (cons 90 3)
          (cons 70 0)
        )
        (mapcar '(lambda (pt) (cons 10 pt)) (list center p1 p2))
      )
    )
  )

  ;; 이름 텍스트 생성
  (entmakex
    (list '(0 . "TEXT") (cons 8 "맨홀설명선") (cons 62 4)
          (cons 10 namePt) (cons 40 textHeight) (cons 1 name) (cons 41 widthFactor)
          (cons 7 "Standard") (cons 72 0) (cons 73 0) (cons 11 namePt))
  )

  ;; 고도 텍스트 생성
  (if (/= elev "")
    (entmakex
      (list '(0 . "TEXT") (cons 8 "맨홀설명선") (cons 62 4)
            (cons 10 elevPt) (cons 40 textHeight) (cons 1 elev) (cons 41 widthFactor)
            (cons 7 "Standard") (cons 72 0) (cons 73 0) (cons 11 elevPt))
    )
  )

  ;; 마지막 점 수정
  (if lineEnt
    (progn
      (setq entData (entget lineEnt))
      (setq newEntData (subst (cons 10 p2) (last (vl-remove-if-not '(lambda (x) (= (car x) 10)) entData)) entData))
      (entmod newEntData)
    )
  )

  p2
)

(defun c:p_mhlabel ( / ss i ent typ circles lines txts circle center pt1 pt2 otherPt line1 line2 closestText minDist txtEnt txtPt dist txtVal entData a1 a2 doc ms parsed txtData txtPos projPt projDist p2 textHeight)

  ;; 레이어 생성
  (if (not (tblsearch "LAYER" "맨홀설명선"))
    (entmakex (list '(0 . "LAYER") (cons 2 "맨홀설명선") (cons 62 4)))
  )

  ;; 객체 선택
  (prompt "\n설명선을 생성할 영역을 드래그해서 선택하세요.")
  (setq ss (ssget '((0 . "CIRCLE,LINE"))))

  (if (not ss)
    (prompt "\n[!] 객체 선택 안됨.")
    (progn
      (prompt "\n텍스트 높이를 입력하세요 (기본값 3.0): ")
      (setq textHeight (getreal))
      (if (not textHeight) (setq textHeight 3.0))

      (setq circles '() lines '())
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq typ (cdr (assoc 0 (entget ent))))
        (cond
          ((= typ "CIRCLE") (setq circles (cons ent circles)))
          ((= typ "LINE")   (setq lines   (cons ent lines)))
        )
        (setq i (1+ i))
      )

      ;; 텍스트 수집
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq ms (vla-get-ModelSpace doc))
      (setq txts '())
      (vlax-for obj ms
        (if (member (vla-get-ObjectName obj) '("AcDbText" "AcDbMText"))
          (setq txts (cons (vlax-vla-object->ename obj) txts))
        )
      )

      ;; 설명선 생성 루프
      (foreach circle circles
        (setq entData (entget circle))
        (setq center (cdr (assoc 10 entData)))

        (setq line1 nil)
        (foreach ln lines
          (setq entData (entget ln))
          (setq pt1 (cdr (assoc 10 entData)))
          (setq pt2 (cdr (assoc 11 entData)))
          (if (and pt1 pt2 (< (distance pt1 center) 1.0) (not line1))
            (setq line1 ln otherPt pt2)
            (if (< (distance pt2 center) 1.0) (setq line1 ln otherPt pt1))
          )
        )

        (setq line2 nil)
        (foreach ln lines
          (if (and line1 (not (equal ln line1)))
            (progn
              (setq entData (entget ln))
              (setq a1 (cdr (assoc 10 entData)))
              (setq a2 (cdr (assoc 11 entData)))
              (if (or (< (distance a1 otherPt) 1.0) (< (distance a2 otherPt) 1.0))
                (setq line2 ln)
              )
            )
          )
        )

        (if (and line2)
          (progn
            (setq minDist 1e9 closestText nil)
            (foreach txtEnt txts
              (setq txtData (entget txtEnt))
              (setq txtPos (cdr (assoc 10 txtData)))
              (if txtPos
                (progn
                  (setq projPt (vlax-curve-getClosestPointTo line2 txtPos))
                  (setq projDist (distance txtPos projPt))
                  (if (< projDist minDist)
                    (progn
                      (setq minDist projDist)
                      (setq closestText txtEnt)
                    )
                  )
                )
              )
            )

            (if closestText
              (progn
                (setq parsed (getTextParts (cdr (assoc 1 (entget closestText)))))
                (if parsed
                  (setq p2 (drawLeaderLine center (car parsed) (cadr parsed) textHeight))
                )
              )
            )
          )
        )
      )
      (prompt "\n[✔] 설명선 및 길이 조정 완료.")
    )
  )
  (princ)
)

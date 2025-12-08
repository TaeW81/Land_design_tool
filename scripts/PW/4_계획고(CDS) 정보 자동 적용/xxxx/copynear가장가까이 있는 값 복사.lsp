(defun c:FHEDIT (/ ss i ent obj pt ss2 j cand cobj ctext cpt dist mindist closest newtext)
  (vl-load-com)

  ;; 전체 선택
  (setq ss (ssget "_:L" '((0 . "TEXT"))))
  (if (not ss)
    (progn (prompt "\n오류: 선택된 텍스트가 없습니다.") (exit))
  )

  ;; 모든 텍스트 순회
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq pt  (vlax-get obj 'InsertionPoint))

    ;; L_FH 레이어의 텍스트만 처리
    (if (equal (strcase (vla-get-layer obj)) "L_FH")
      (progn
        ;; 후보 중 가장 가까운 FH: 텍스트 찾기
        (setq j 0
              mindist 1e99
              closest nil)

        (while (< j (sslength ss))
          (setq cand (ssname ss j))
          (setq j (1+ j))
          (if (not (eq ent cand)) ; 자기 자신 제외
            (progn
              (setq cobj  (vlax-ename->vla-object cand))
              (setq ctext (vla-get-TextString cobj))
              (if (wcmatch ctext "FH:*")
                (progn
                  (setq cpt (vlax-get cobj 'InsertionPoint))
                  (setq dist (distance pt cpt))
                  (if (< dist mindist)
                    (progn
                      (setq mindist dist)
                      (setq closest cobj)
                    )
                  )
                )
              )
            )
          )
        )

        ;; 가장 가까운 FH 텍스트에서 FH: 제거하여 대입
        (if closest
          (progn
            (setq newtext (substr (vla-get-TextString closest) 4)) ; "FH:" 제거
            (vla-put-TextString obj newtext)
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (prompt "\n모든 L_FH 텍스트가 근처 FH: 텍스트로 변경되었습니다.")
  (princ)
)

(defun c:copynear ( / ss srcEnt dstEnt srcLayer dstLayer srcList dstList i obj lay )
  (vl-load-com)
  ;; 전체 TEXT 객체 선택
  (prompt "\n전체 TEXT 객체를 선택하세요.")
  (setq ss (ssget '((0 . "TEXT"))))
  (if (not ss)
    (prompt "\nTEXT 객체를 선택해주세요.")
    (progn
      ;; 복사할(참조) 레이어 객체 하나 선택
      (prompt "\n복사할(참조) 레이어의 TEXT 객체를 하나 선택하세요.")
      (setq srcEnt (ssget "_+.:S" '((0 . "TEXT"))))
      (if (not srcEnt)
        (prompt "\n복사할 레이어의 객체를 선택해주세요.")
        (progn
          (setq srcLayer (strcase (vla-get-layer (vlax-ename->vla-object (ssname srcEnt 0)))))
          ;; 붙여넣을(대상) 레이어 객체 하나 선택
          (prompt "\n붙여넣을(대상) 레이어의 TEXT 객체를 하나 선택하세요.")
          (setq dstEnt (ssget "_+.:S" '((0 . "TEXT"))))
          (if (not dstEnt)
            (prompt "\n붙여넣을 레이어의 객체를 선택해주세요.")
            (progn
              (setq dstLayer (strcase (vla-get-layer (vlax-ename->vla-object (ssname dstEnt 0)))))
              ;; 레이어별 객체 분류
              (setq srcList '() dstList '())
              (setq i 0)
              (while (< i (sslength ss))
                (setq obj (vlax-ename->vla-object (ssname ss i)))
                (setq lay (strcase (vla-get-layer obj)))
                (if (equal lay srcLayer)
                  (setq srcList (cons obj srcList))
                )
                (if (equal lay dstLayer)
                  (setq dstList (cons obj dstList))
                )
                (setq i (1+ i))
              )
              ;; 붙여넣기 레이어 객체마다 가장 가까운 복사 레이어 텍스트 찾아 값 복사
              (foreach dstObj dstList
                (setq dstPt (vlax-get dstObj 'InsertionPoint))
                (setq mindist 1e99 closest nil)
                (foreach srcObj srcList
                  (setq srcPt (vlax-get srcObj 'InsertionPoint))
                  (setq dist (distance dstPt srcPt))
                  (if (< dist mindist)
                    (progn
                      (setq mindist dist)
                      (setq closest srcObj)
                    )
                  )
                )
                (if closest
                  (progn
                    (setq srcText (vla-get-TextString closest))
                    (vla-put-TextString dstObj srcText)
                  )
                )
              )
              (prompt "\n복사가 완료되었습니다.")
            )
          )
        )
      )
    )
  )
  (princ)
)

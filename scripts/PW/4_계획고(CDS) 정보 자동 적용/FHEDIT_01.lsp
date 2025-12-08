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

;; ----------------------------------------------------------------------
;; [KAB] 키맵 자동 삽입 리습
;;
;; 이 리습은 동일한 키맵을 여러 도각(XREF 또는 블록) 내부에 자동으로 삽입해주는 도구입니다.
;; 사용자는 키맵 객체들을 드래그하여 선택하고, 도각 내 어디에 키맵이 배치될지 기준이 되는 좌표를 직접 지정합니다.
;;
;; ⚠️ 기준점을 설정할 때 반드시 "가장 왼쪽 아래에 위치한 도각"을 기준으로 클릭해야 합니다.
;; 기준점은 키맵 내에서 도각의 삽입점과 맞추고 싶은 지점을 뜻합니다.
;;
;; [사용 방법]
;; 1. 명령어 실행: KAB
;; 2. 키맵 객체들을 드래그 선택
;; 3. 키맵 기준점 클릭 (예: 키맵의 좌하단 모서리)
;; 4. 전체 도각 블록들을 드래그 선택 (XREF 포함)
;; 5. 각 도각의 동일한 위치에 키맵 자동 삽입 완료
;; ----------------------------------------------------------------------

(defun c:KAB (/ ss_blocks ss_keymap ent keymapObjs basePt
                           doc entList blkName insertPt minPt maxPt)

  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))

  (prompt "\n[KEYMAP 반복 삽입 - 모든 블록 포함]")

  ;; 1. 키맵으로 사용할 객체 드래그 선택
  (prompt "\n키맵으로 사용할 객체들을 드래그로 선택하세요: ")
  (setq ss_keymap (ssget))
  (if (not ss_keymap)
    (progn (prompt "\n[오류] 키맵 객체 선택이 취소되었습니다.") (exit))
  )

  ;; 2. 키맵 기준점 선택
  (prompt "\n키맵에서 기준이 되는 점을 마우스로 찍으세요: ")
  (setq basePt (getpoint "\n기준점: "))
  (if (not basePt)
    (progn (prompt "\n[오류] 기준점 선택이 취소되었습니다.") (exit))
  )

  ;; 3. 도각 블록들 전체 선택
  (prompt "\n도각 블록(XREF 포함)을 드래그로 전체 선택하세요: ")
  (setq ss_blocks (ssget "_:L" '((0 . "INSERT"))))
  (if (not ss_blocks)
    (progn (prompt "\n[오류] 블록이 선택되지 않았습니다.") (exit))
  )

  ;; 4. 반복 삽입
  (repeat (sslength ss_blocks)
    (setq ent (ssname ss_blocks 0))
    (setq ss_blocks (ssdel ent ss_blocks))
    (setq entList (entget ent))
    (setq insertPt (cdr (assoc 10 entList))) ;; 블록의 삽입점

    ;; 키맵 복사 & 붙여넣기
    (command "_copybase" basePt ss_keymap "")
    (command "_pasteclip" insertPt)
  )

  (prompt "\n[완료] 키맵이 모든 도각 블록에 삽입되었습니다.")
  (princ)
)

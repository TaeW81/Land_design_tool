;;; PROFILEEDIT - 종단면도 선분 이동 및 중심 기준 텍스트 회전 리습
;; 작성 프롬프트:
;; "선분 클릭 시, 양 끝점의 숫자 크기를 비교해 상하 4m 이동시키고,
;;  중심 기준 상하 7m 이내에 위치한 TEXT 객체를 수정된 선의 기울기에 맞게 회전시키되,
;;  기존 회전값이 90도인 텍스트는 제외하고,
;;  회전 대상 TEXT가 3개 이상일 경우 회전을 수행하지 않도록 해줘"

;;; 명령어: PROFILEEDIT
;;; 설명:
;;; - 종단면도의 선분 하나를 선택하여 양 끝점의 수치(TEXT)를 비교하고,
;;;   큰 값 쪽은 위로 4m, 작은 값 쪽은 아래로 4m 이동시킵니다.
;;; - 이동된 선의 중심점을 기준으로, 상하 7m 이내에 있는 TEXT 객체들을 찾아
;;;   선의 기울기(angle)에 맞춰 회전시킵니다.
;;; - 단, 기존 회전각도가 정확히 90도인 텍스트는 회전 대상에서 제외됩니다.
;;; - 또한, 회전 대상 TEXT가 3개 이상인 경우 회전 작업을 생략합니다.
;;; - 단위는 도면 단위 기준입니다.
;;; - MTEXT는 미지원, TEXT만 회전 처리됩니다.

(defun c:p_profedit (/ ent data pt1 pt2 info1 info2 val1 val2 ang mid ss idx ed ins rot rotTargets
                        len ptV ptH upTexts downTexts txt txt_y_up_offset txt_y_down_offset)
  (vl-load-com)
  (prompt "\n[PROFILEEDIT 명령 실행됨]")
  (prompt "\n종단면도 라인을 선택하세요.")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq data  (entget ent)
            pt1   (cdr (assoc 10 data))
            pt2   (cdr (assoc 11 data))
            info1 (getNearestNumericTextInfo pt1)
            info2 (getNearestNumericTextInfo pt2))
      (if (and info1 info2)
        (progn
          (setq val1 (car info1)
                val2 (car info2))
          (setq len (dist2D pt1 pt2)) ;; 기존 길이 저장

          ;; 끝점 이동
          (cond
            ((> val1 val2)
             (setq pt1 (offsetPoint pt1 4)
                   pt2 (offsetPoint pt2 -4)))
            ((< val1 val2)
             (setq pt1 (offsetPoint pt1 -4)
                   pt2 (offsetPoint pt2 4)))
          )

          ;; 선 수정
          (modifyLine ent pt1 pt2)

          ;; 중심 및 기울기 계산
          (setq ang (angle pt1 pt2))
          (setq mid (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))

          ;; 중심 기준으로 인근 TEXT 선택
          (setq ss (ssget "_W"
                          (list (- (car mid) 20.0) (- (cadr mid) 7.0))
                          (list (+ (car mid) 20.0) (+ (cadr mid) 7.0))
                          '((0 . "TEXT"))))
          (setq rotTargets '())
          (if ss
            (progn
              (setq idx 0)
              (while (< idx (sslength ss))
                (setq ed (entget (ssname ss idx)))
                (setq ins (cdr (assoc 10 ed)))
                (setq rot (cdr (assoc 50 ed)))
                (if (and ins (<= (abs (- (cadr ins) (cadr mid))) 7.0)
                         (/= (abs rot) (/ pi 2))) ;; 90도 제외
                  (setq rotTargets (cons ed rotTargets)))
                (setq idx (1+ idx))
              )

              (if (< (length rotTargets) 3)
                ;; 회전 처리
                (foreach ed rotTargets
                  (entmod (subst (cons 50 ang) (assoc 50 ed) ed)))
                ;; 텍스트가 3개 이상이더라도, 길이가 15 이하일 때만 재배치
                (if (<= len 15.0)
                  (progn
                    (prompt "\n※ 선 길이 15 이하이므로 텍스트 재배치합니다.")
                    (setq txt_y_up_offset 0.0 txt_y_down_offset 0.0)

                    ;; 수직선 + 수평선 생성
                    (setq ptV (list (car mid) (+ (cadr mid) 40.0) 0.0))
                    (entmakex (list (cons 0 "LINE")
                                    (cons 10 mid)
                                    (cons 11 ptV)
                                    (cons 62 7))) ;; 빨강
                    (setq ptH (list (+ (car ptV) 20.0) (cadr ptV) 0.0))
                    (entmakex (list (cons 0 "LINE")
                                    (cons 10 ptV)
                                    (cons 11 ptH)
                                    (cons 62 7))) ;; 빨강

                    ;; 텍스트 삭제 + 재작성
                    (foreach ed rotTargets
                      (setq txt (cdr (assoc 1 ed)))
                      (entdel (cdr (assoc -1 ed)))
                      (cond
                        ((wcmatch txt "S*")
                         (entmakex
                           (list
                             (cons 0 "TEXT")
                             (cons 10 (list (car mid) (- (+ (cadr mid) 41.5 txt_y_up_offset) 1.0) 0.0))
                             (cons 40 2.5)
                             (cons 1 txt)
                             (cons 7 "STANDARD")
                             (cons 62 7) ;; 흰색
                           )
                         )
                         (setq txt_y_up_offset (+ txt_y_up_offset 3.0))
                        )
                        ((or (wcmatch txt "H*") (wcmatch txt "L*"))
                         (entmakex
                           (list
                             (cons 0 "TEXT")
                             (cons 10 (list (car mid) (- (+ (cadr mid) 38.5 (- txt_y_down_offset)) 1.0) 0.0))
                             (cons 40 2.5)
                             (cons 1 txt)
                             (cons 7 "STANDARD")
                             (cons 62 7) ;; 흰색
                           )
                         )
                         (setq txt_y_down_offset (+ txt_y_down_offset 3.0))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (prompt "\n양 끝점 근처에서 숫자 텍스트를 찾을 수 없습니다.")
      )
    )
    (prompt "\n선택된 객체가 없습니다.")
  )
  (princ)
)

;; 숫자 텍스트 찾기 함수
(defun getNearestNumericTextInfo (pt / ss idx ent ed txt val ins dist bestVal bestEnt bestDist)
  (setq ss (ssget "_C"
                  (list (- (car pt) 10.0) (- (cadr pt) 10.0))
                  (list (+ (car pt) 10.0) (+ (cadr pt) 10.0))
                  '((0 . "TEXT"))))
  (setq idx 0)
  (while (and ss (< idx (sslength ss)))
    (setq ent (ssname ss idx)
          ed  (entget ent)
          txt (cdr (assoc 1 ed))
          val (distof txt 2))
    (if val
      (progn
        (setq ins (cdr (assoc 10 ed))
              dist (distance pt ins))
        (if (< dist 10.0)
          (if (or (not bestDist) (< dist bestDist))
            (setq bestDist dist bestVal val bestEnt ent)))))
    (setq idx (1+ idx)))
  (if bestVal (list bestVal bestEnt) nil)
)

(defun offsetPoint (pt dy)
  (list (car pt) (+ (cadr pt) dy) (if (caddr pt) (caddr pt) 0.0)))

(defun modifyLine (ent newPt1 newPt2 / obj)
  (setq obj (vlax-ename->vla-object ent))
  (vla-put-StartPoint obj (vlax-3d-point newPt1))
  (vla-put-EndPoint obj (vlax-3d-point newPt2)))

(defun dist2D (p1 p2)
  (distance (list (car p1) (cadr p1)) (list (car p2) (cadr p2))))

(princ "\nPROFILEEDIT 리습이 로드되었습니다. 'PROFILEEDIT' 명령을 실행하세요.")
(princ)

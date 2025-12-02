(defun c:renametext ( / ss cnt ent ename txt newtxt prefix padlen numstr )
  (setq prefix (getstring "\n머리말을 입력하세요: ")) ; 사용자에게 머리말 입력 받기
  (setq ss (ssget '((0 . "TEXT")))) ; TEXT 객체만 선택
  (if ss
    (progn
      (setq padlen (if (<= (sslength ss) 100) 2 3)) ; 100개 이하면 2자리, 이상이면 3자리
      (setq cnt 1)
      (repeat (sslength ss)
        (setq ename (ssname ss (1- cnt)))
        (setq ent (entget ename))
        (setq numstr (substr (strcat (make-string padlen #\0) (itoa cnt)) (- (+ (strlen (itoa cnt)) padlen) (strlen (itoa cnt)))))
        (setq newtxt (strcat prefix numstr)) ; 머리말과 번호 사이에 _ 없이
        (setq ent (subst (cons 1 newtxt) (assoc 1 ent) ent))
        (entmod ent)
        (entupd ename)
        (setq cnt (1+ cnt))
      )
      (princ (strcat "\n총 " (itoa (sslength ss)) "개의 TEXT가 변경되었습니다."))
    )
    (prompt "\nTEXT 객체를 선택해주세요.")
  )
  (princ)
)
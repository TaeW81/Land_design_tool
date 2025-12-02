(defun make-string (n ch / s)
  "Make a string of length n of the ASCII char ch."
  (setq s "")
  (while (> n 0)
    (setq s (strcat s (chr ch)))
    (setq n (1- n))
  )
  s
)

(defun c:renametext ( / prefix ss total padlen idx entname entdata padded numstr newtxt )
  ;; 머리말 입력
  (setq prefix (getstring "\n머리말을 입력하세요: "))
  ;; TEXT 객체만 선택
  (setq ss (ssget '((0 . "TEXT"))))
  (if (not ss)
      (progn
        (prompt "\nTEXT 객체를 선택해주세요.")
        (princ)
      )
      (progn
        ;; 총 객체 수
        (setq total (sslength ss))
        ;; padlen: 100개 이하 2자리, 그 이상 3자리
        (setq padlen (if (<= total 100) 2 3))
        (setq idx 1)
        (while (<= idx total)
          (setq entname (ssname ss (1- idx))
                entdata (entget entname)
          )
          ;; 번호 문자열 생성 및 앞 패딩
          (setq padded (strcat (make-string padlen 48) (itoa idx)))
          ;; 마지막 padlen 자리만 추출
          (setq numstr (substr padded (+ 1 (- (strlen padded) padlen)) padlen))
          ;; 머리말과 번호 결합
          (setq newtxt (strcat prefix numstr))
          ;; 텍스트 내용 변경
          (setq entdata (subst (cons 1 newtxt) (assoc 1 entdata) entdata))
          (entmod entdata)
          (entupd entname)
          (setq idx (1+ idx))
        )
        (prompt (strcat "\n총 " (itoa total) "개의 TEXT가 변경되었습니다."))
        (princ)
      )
  )
)
(princ "\nrenametext LISP 로드 완료.")

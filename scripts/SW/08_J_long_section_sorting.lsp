나의 말:
(defun c:p_profsort ( / p1 p2 recs idx cnt rec
                       extents minx miny maxx maxy
                       allTxts j txtEnt pt txtval rname sepPos
                       ssInside entList ent2 data groups grpEntry
                       spacingX spacingY row col items maxh
                       w h newX newY dx dy ssMove
                       grpKey sortKey parsed grpMap)

  (vl-load-com)
  (setq spacingX 20.0
        spacingY 50.0)

  (setq p1   (getpoint "\n▶ 첫 번째 코너 선택: "))
  (setq p2   (getcorner p1 "\n▶ 두 번째 코너 선택: "))
  (setq recs (ssget "C" p1 p2 '((0 . "LWPOLYLINE,POLYLINE") (70 . 1))))
  (if (not recs)
    (progn (princ "\n>> 선택된 박스가 없습니다.") (exit)))

  (setq allTxts (ssget "_X" '((0 . "TEXT,MTEXT"))))
  (setq groups '() idx 0 cnt (sslength recs))

  ;; 보조: 그룹 키와 정렬 숫자 추출 함수
  (defun parse-name (s / base num)
    ;; “중(집)탕3-16-우수종단도면” → ("중(집)탕3" 16)
    (if (vl-string-search "-우수" s)
      (setq s (substr s 1 (vl-string-search "-우수" s)))) ; 뒤 "-우수" 제거
    (if (and (setq sepPos (vl-string-search "-" s))
             (setq base (substr s 1 sepPos))
             (setq numStr (substr s (+ sepPos 2)))
             (setq num (atoi numStr)))
      (list base num)
      (list s 0))) ; fallback

  (while (< idx cnt)
    (setq rec     (ssname recs idx)
          extents (rect-extents rec)
          minx    (nth 0 extents)
          miny    (nth 1 extents)
          maxx    (nth 2 extents)
          maxy    (nth 3 extents))

    (setq j 0 txtEnt nil)
    (while (and (< j (sslength allTxts)) (not txtEnt))
      (setq txtEnt (ssname allTxts j)
            pt     (cdr (assoc 10 (entget txtEnt))))
      (if (not (and (>= (car pt) minx) (<= (car pt) maxx)
                    (>= (cadr pt) miny) (<= (cadr pt) maxy)))
        (setq txtEnt nil))
      (setq j (1+ j)))

    (if txtEnt
      (progn
        (setq txtval (cdr (assoc 1 (entget txtEnt))))
        (setq parsed (parse-name txtval)
              grpKey (car parsed)
              sortKey (cadr parsed))

        ;; 박스 안 객체들
        (setq ssInside (ssget "W" (list minx miny 0.0) (list maxx maxy 0.0)))
        (setq entList '() k 0)
        (while (< k (sslength ssInside))
          (setq ent2 (ssname ssInside k))
          (if (not (eq ent2 rec))
            (setq entList (cons ent2 entList)))
          (setq k (1+ k)))

        (setq w    (- maxx minx)
              h    (- maxy miny)
              data (list sortKey entList rec w h minx miny))

        (setq grpEntry (assoc grpKey groups))
        (if grpEntry
          (setq groups
            (mapcar (function
              (lambda (g)
                (if (equal (car g) grpKey)
                  (cons grpKey (append (cdr g) (list data)))
                  g))) groups))
          (setq groups (cons (cons grpKey (list data)) groups)))))
    (setq idx (1+ idx)))

  ;; 그룹별 정렬
  (setq row 0)
  (foreach grp groups
    (setq items (cdr grp)
          maxh  0
          col   0)
    ;; sort by sortKey
    (setq items (vl-sort items '(lambda (a b) (< (car a) (car b)))))
    (foreach it items
      (setq maxh (max maxh (nth 3 it))))
    (foreach it items
      (setq sortKey (nth 0 it)
            entList (nth 1 it)
            rec     (nth 2 it)
            w       (nth 3 it)
            h       (nth 4 it)
            minx    (nth 5 it)
            miny    (nth 6 it)
            newX    (+ spacingX (* col (+ w spacingX)))
            newY    (- (* row (+ maxh spacingY)))
            dx      (- newX minx)
            dy      (- newY miny)
            ssMove  (ssadd rec))
      (foreach e entList (setq ssMove (ssadd e ssMove)))
      (command "_.MOVE" ssMove "" "0,0" (strcat (rtos dx 2 4) "," (rtos dy 2 4)))
      (setq col (1+ col)))
    (setq row (1+ row)))

  (princ "\n>> 도로명 + 번호 기준 정렬 완료!")
  (princ))

;; 닫힌 폴리라인의 extents 계산 함수
(defun rect-extents (ent / ed coords xs ys minx maxx miny maxy)
  (setq ed (entget ent) coords '())
  (foreach d ed
    (if (= (car d) 10)
      (setq coords (cons (cdr d) coords))))
  (setq xs (mapcar 'car coords)
        ys (mapcar 'cadr coords)
        minx (apply 'min xs)
        maxx (apply 'max xs)
        miny (apply 'min ys)
        maxy (apply 'max ys))
  (list minx miny maxx maxy))
;;; arrprof.lsp
;;; — 박스 안 모든 객체를 한 번에 옮기며 “XXX - 우수종단면도” 앞의 XXX 로 그룹핑

(defun c:ARRPROF ( / p1 p2 recs idx cnt rec
                       extents minx miny maxx maxy
                       allTxts j txtEnt pt txtval rname sepPos
                       ssInside entList ent2 data groups grpEntry
                       spacingX spacingY row col items maxh
                       w h newX newY dx dy ssMove)

  ;; ■ 간격 설정 ■
  (setq spacingX 20.0   ; 같은 그룹 내 가로 간격
        spacingY 50.0)  ; 그룹 간 세로 간격

  ;; 1) 드래그(크로싱 윈도우)로 박스들 선택
  (setq p1   (getpoint "\n▶ 첫 번째 코너 선택: "))
  (setq p2   (getcorner p1 "\n▶ 두 번째 코너 선택: "))
  (setq recs
        (ssget "C" p1 p2
               '((0 . "LWPOLYLINE,POLYLINE")
                 (70 . 1)))  ; 닫힌 폴리라인만
  )
  (if (not recs)
      (progn (princ "\n>> 선택된 박스가 없습니다.") (exit)))

  ;; 2) 전체 문서의 TEXT/MTEXT 취합 (그룹키 추출용)
  (setq allTxts (ssget "_X" '((0 . "TEXT,MTEXT"))))

  ;; 3) 그룹 리스트 초기화
  (setq groups '()
        idx    0
        cnt    (sslength recs))

  ;; 4) 각 박스마다 처리
  (while (< idx cnt)
    ;; 4-1) 박스와 경계(extents) 계산
    (setq rec     (ssname recs idx)
          extents (rect-extents rec)
          minx    (nth 0 extents)
          miny    (nth 1 extents)
          maxx    (nth 2 extents)
          maxy    (nth 3 extents))

    ;; 4-2) Extents 안 첫 번째 TEXT/MTEXT 찾아 txtEnt 지정
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
        ;; 4-3) “XXX - 우수종단면도” 에서 “XXX” 만 추출
        (setq txtval (cdr (assoc 1 (entget txtEnt))))
        (if (setq sepPos (vl-string-search " - " txtval))
            (setq rname (substr txtval 1 sepPos))
            (setq rname txtval))
        (setq rname (vl-string-trim " " rname))

        ;; 4-4) 박스 안 모든 객체(Window 선택), rec 제외
        (setq ssInside
              (ssget "W"
                     (list minx miny 0.0)
                     (list maxx maxy 0.0)))
        (setq entList '() k 0)
        (while (< k (sslength ssInside))
          (setq ent2 (ssname ssInside k))
          (if (not (eq ent2 rec))
              (setq entList (cons ent2 entList)))
          (setq k (1+ k)))

        ;; 4-5) 이동 정보 준비
        (setq w    (- maxx minx)
              h    (- maxy miny)
              data (list entList rec w h minx miny))

        ;; 4-6) groups에 추가
        (setq grpEntry (assoc rname groups))
        (if grpEntry
            ;; 이미 있으면 뒤에 붙임
            (setq groups
                  (mapcar
                    (function
                      (lambda (g)
                        (if (equal (car g) rname)
                            (cons rname (append (cdr g) (list data)))
                            g)))
                    groups))
            ;; 새 그룹 생성
            (setq groups (cons (cons rname (list data)) groups)))
      ))
    (setq idx (1+ idx)))

  ;; 5) 그룹별 가로·세로 정렬
  (setq row 0)
  (foreach grp groups
    (setq items (cdr grp)
          maxh  0
          col   0)
    ;; 그룹 내 최대 높이 계산
    (foreach it items
      (setq maxh (max maxh (nth 3 it))))
    ;; 각 박스+객체를 한 번에 MOVE
    (foreach it items
      (setq entList (nth 0 it)
            rec     (nth 1 it)
            w       (nth 2 it)
            h       (nth 3 it)
            minx    (nth 4 it)
            miny    (nth 5 it)
            newX    (+ spacingX (* col (+ w spacingX)))
            newY    (- (* row (+ maxh spacingY)))
            dx      (- newX minx)
            dy      (- newY miny)
            ssMove  (ssadd rec))
      ;; 모든 객체를 ssMove에 추가
      (foreach e entList (setq ssMove (ssadd e ssMove)))
      ;; 한 번의 MOVE 명령으로 이동
      (command "_.MOVE" ssMove "" "0,0" (strcat (rtos dx 2 4) "," (rtos dy 2 4)))
      (setq col (1+ col)))
    (setq row (1+ row)))

  (princ "\n>> 그룹별 정렬 완료!")
  (princ)
)

;;; 보조함수: 닫힌 폴리라인 Extents 계산
(defun rect-extents (ent / ed coords xs ys minx maxx miny maxy)
  (setq ed     (entget ent)
        coords '())
  (foreach d ed
    (if (= (car d) 10)
        (setq coords (cons (cdr d) coords))))
  (setq xs   (mapcar 'car coords)
        ys   (mapcar 'cadr coords)
        minx (apply 'min xs) maxx (apply 'max xs)
        miny (apply 'min ys) maxy (apply 'max ys))
  (list minx miny maxx maxy))

(princ)

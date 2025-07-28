;; PLN 명령어 - 방류부 관로를 기준으로 역방향으로 연결된 순서로 번호 부여
(defun c:p_pipeauto (/ get-angle distance pt-close?
                selBoundary ss ht i en obj startPt endPt angle midpt totalLength
                lineCounter lineLabel prefix lenText selOutlet outletPt
                boundaryPts n boundaryPtList distList boundaryHandle
                segList connList visited order queue handle seg nextHandle dist
                excludeSet excludeHandles exLen outletHandle startPt endPt allHandles)

  (vl-load-com)

  ;; 각도 계산 함수
  (defun get-angle (p1 p2)
    (atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1))))

  ;; 거리 계산 함수
  (defun distance (p1 p2)
    (sqrt (+ (expt (- (car p1) (car p2)) 2)
             (expt (- (cadr p1) (cadr p2)) 2))))

  ;; 좌표 근접 비교 함수
  (defun pt-close? (p1 p2 / eps)
    (setq eps 0.01)
    (< (distance p1 p2) eps))

  ;; 텍스트 생성 함수
  (defun make-text (txt pt lay rot justH justV)
    (entmakex
      (list
        (cons 0 "TEXT")
        (cons 10 pt)
        (cons 11 pt)
        (cons 40 ht)
        (cons 1 txt)
        (cons 8 lay)
        (cons 7 "Standard")
        (cons 50 rot)
        (cons 72 justH)
        (cons 73 justV)
        (cons 41 1.0))))

  ;; 텍스트 높이
  (setq ht (getreal "\n텍스트 높이를 입력하세요 <2.5>: "))
  (if (not ht) (setq ht 2.5))

  ;; 접두사 입력
  (setq prefix (getstring T "\nLine 텍스트의 접두사(예: A): "))
  (if (not prefix) (setq prefix ""))

  ;; 외곽 폴리선 선택
  (prompt "\n외곽 폴리선을 선택하세요: ")
  (setq selBoundary (car (entsel)))
  (if selBoundary
    (progn
      (setq boundaryObj (vlax-ename->vla-object selBoundary))
      (setq boundaryHandle (cdr (assoc 5 (entget selBoundary))))
      (setq boundaryPts (vlax-get boundaryObj 'coordinates))
      (setq n 0 boundaryPtList '())
      (while (< n (length boundaryPts))
        (setq boundaryPtList (cons (list (nth n boundaryPts) (nth (1+ n) boundaryPts)) boundaryPtList))
        (setq n (+ n 2)))
      (setq boundaryPtList (reverse boundaryPtList))

      ;; 방류부 선택
      (prompt "\n방류부 선을 클릭하세요: ")
      (setq selOutlet (car (entsel)))
      (if selOutlet
        (progn
          (setq outletHandle (cdr (assoc 5 (entget selOutlet))))
          (setq outletObj (vlax-ename->vla-object selOutlet))
          (setq outletPt (vlax-curve-getPointAtDist outletObj (/ (vlax-curve-getDistAtParam outletObj (vlax-curve-getEndParam outletObj)) 2.0)))

          ;; 내부 관로 수집
          (setq ss (ssget "CP" boundaryPtList '((0 . "LWPOLYLINE,LINE"))))
          (if ss
            (progn
              ;; 제외할 관로 선택
              (prompt "\n제외할 관로가 있다면 선택하세요. 없으면 Enter: ")
              (setq excludeSet (ssget))
              (setq excludeHandles '())
              (if excludeSet
                (progn
                  (setq exLen (sslength excludeSet) i 0)
                  (while (< i exLen)
                    (setq excludeHandles (cons (cdr (assoc 5 (entget (ssname excludeSet i)))) excludeHandles))
                    (setq i (1+ i)))))

              ;; 세그먼트 리스트 구성
              (setq i 0 segList '() allHandles '())
              (while (< i (sslength ss))
                (setq en (ssname ss i))
                (setq handle (cdr (assoc 5 (entget en))))
                (if (and (/= handle boundaryHandle)
                         (not (member handle excludeHandles)))
                  (progn
                    (setq obj (vlax-ename->vla-object en))
                    (setq totalLength (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj)))
                    (setq startPt (vlax-curve-getStartPoint obj))
                    (setq endPt   (vlax-curve-getEndPoint obj))
                    (setq midpt   (vlax-curve-getPointAtDist obj (/ totalLength 2.0)))
                    (setq dist    (distance outletPt midpt))
                    (setq segList (cons (list handle en obj startPt endPt midpt totalLength dist) segList))
                    (setq allHandles (cons handle allHandles))))
                (setq i (1+ i)))

              ;; 연결 정보 수집
              (setq connList '())
              (foreach seg1 segList
                (setq conn '())
                (setq h1 (car seg1) s1 (nth 3 seg1) e1 (nth 4 seg1))
                (foreach seg2 segList
                  (setq h2 (car seg2) s2 (nth 3 seg2) e2 (nth 4 seg2))
                  (if (and (/= h1 h2)
                           (or (pt-close? s1 s2) (pt-close? s1 e2)
                               (pt-close? e1 s2) (pt-close? e1 e2)))
                    (setq conn (cons h2 conn))))
                (setq connList (cons (cons h1 conn) connList)))

              ;; 방류부 기준으로 연결 순서 수집
              (setq visited '() order '() queue (list outletHandle))
              (while queue
                (setq handle (car queue)) (setq queue (cdr queue))
                (if (not (member handle visited))
                  (progn
                    (setq visited (cons handle visited))
                    (setq order (append (list handle) order))
                    (setq nexts (cdr (assoc handle connList)))
                    (foreach nh nexts
                      (if (not (member nh visited))
                        (setq queue (append queue (list nh))))))))

              ;; 남은 관로 처리
              (foreach h allHandles
                (if (not (member h order))
                  (setq order (cons h order))))

              ;; 텍스트 생성
              (setq lineCounter 1)
              (foreach h order
                (setq seg (assoc h segList))
                (setq en (nth 1 seg) obj (nth 2 seg)
                      startPt (nth 3 seg) endPt (nth 4 seg)
                      midpt (nth 5 seg) totalLength (nth 6 seg))
                (setq angle (get-angle startPt endPt))
                (setq lenText (strcat "L=" (rtos totalLength 2 2) "m"))
                (make-text lenText midpt "L_Diameter" angle 0 3)
                (setq lineLabel (strcat prefix "-" (if (< lineCounter 10) "0" "") (itoa lineCounter)))
                (make-text lineLabel midpt "L_Pipes" angle 0 0)
                (setq lineCounter (1+ lineCounter)))))))))
  (princ))

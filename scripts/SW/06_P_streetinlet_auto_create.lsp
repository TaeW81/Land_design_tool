;; ================== PIWB ==================
;; 기준선(첫 번째 선택)에서 간격마다 '수직선'을 내려,
;; 그 수직선이 대상선(두 번째 선택)과 교차할 때만
;; 연결선과 블록을 생성. 교차가 없으면 건너뜀.
;; 블록 각/위치 계산은 기존 로직 유지.
;; =========================================

(setq blkName nil)
(setq lastInterval 20.0)  ;; 기본 간격값

(defun c:p_stinlet (/ ent1 ent2 obj1 obj2 blkEnt interval dist i param pt1 tdir vdir
                 linept1 linept2 interraw interlist insertPt angle adjPt blkRef tempEnt
                 mode offset blkDef blkWidth blkHeight dx dy isLeft adjX adjY
                 blkItr)

  (vl-load-com)

  ;; 각도 계산 함수
  (defun get-angle (p1 p2)
    (atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))
  )

  ;; safearray/variant → 리스트 변환
  (defun _to-list (v)
    (cond
      ((null v) nil)
      ((= (type v) 'list) v)
      ((= (type v) 'variant) (vlax-safearray->list (vlax-variant-value v)))
      (t nil))
  )

  ;; [x y z x y z ...] → '((x y z) (x y z) ...)
  (defun _chunk3 (lst / r)
    (if (>= (length lst) 3)
      (cons (list (nth 0 lst) (nth 1 lst) (nth 2 lst))
            (_chunk3 (cdddr lst)))
    )
  )

  ;; pt 기준 가장 가까운 점 선택
  (defun _closest (pt pts / best bestd p d)
    (foreach p pts
      (setq d (distance pt p))
      (if (or (null bestd) (< d bestd))
        (setq best p bestd d)))
    best
  )

  ;; ---- 모드 선택 ----
  (prompt "\n[삽입 모드 선택] 1: 설치(정배열) / 2: 설치(중간에서 시작 배열)")
  (initget 1 "1 2")
  (setq mode (getkword "\n모드를 선택하세요 [1/2]: "))
  (if (not mode) (setq mode "1"))

  ;; ---- 블럭 선택/재사용 ----
  (prompt "\n[블럭 선택]")
  (if blkName
    (prompt (strcat "\n이전 선택된 블럭: '" blkName "'"))
    (prompt "\n이전 선택된 블럭: 없음")
  )
  (prompt "\n블럭을 선택하거나 Enter를 눌러 이전 블럭을 사용하세요: ")
  
  ;; 사용자 입력 처리 (블럭 선택 또는 Enter)
  (setq blkEnt (car (entsel)))
  (if blkEnt
    (progn
      ;; 객체가 선택된 경우 - 블럭인지 확인
      (if (eq (cdr (assoc 0 (entget blkEnt))) "INSERT")
        (progn
          (setq blkName (cdr (assoc 2 (entget blkEnt))))
          (if (not (tblsearch "BLOCK" blkName))
            (progn (prompt "\n❌ 유효한 블럭이 아닙니다.") (setq blkName nil) (exit))
            (prompt (strcat "\n✅ 블럭 '" blkName "' 선택됨."))
          )
        )
        (progn
          (prompt "\n❌ 선택된 객체가 블럭이 아닙니다.")
          (if blkName
            (progn
              (prompt (strcat "\n🔄 이전 선택된 블럭 '" blkName "' 사용 중..."))
            )
            (progn
              (prompt "\n❌ 이전 블럭도 없습니다. 블럭을 선택해주세요: ")
              (setq blkEnt (car (entsel)))
              (if (not blkEnt) (progn (prompt "\n❌ 블럭 선택 취소.") (exit)))
              (setq blkName (cdr (assoc 2 (entget blkEnt))))
              (if (not (tblsearch "BLOCK" blkName))
                (progn (prompt "\n❌ 유효한 블럭이 아닙니다.") (setq blkName nil) (exit))
                (prompt (strcat "\n✅ 블럭 '" blkName "' 선택됨."))
              )
            )
          )
        )
      )
    )
    (if blkName
      (prompt (strcat "\n🔄 이전 선택된 블럭 '" blkName "' 사용 중..."))
      (progn 
        (prompt "\n❌ 이전 블럭도 없습니다. 블럭을 선택해주세요: ")
        (setq blkEnt (car (entsel)))
        (if (not blkEnt) (progn (prompt "\n❌ 블럭 선택 취소.") (exit)))
        (setq blkName (cdr (assoc 2 (entget blkEnt))))
        (if (not (tblsearch "BLOCK" blkName))
          (progn (prompt "\n❌ 유효한 블럭이 아닙니다.") (setq blkName nil) (exit))
          (prompt (strcat "\n✅ 블럭 '" blkName "' 선택됨."))
        )
      )
    )
  )

  ;; ---- 블럭 크기(대략) 계산: LINE들만 기준 ----
  (setq blkDef (tblobjname "BLOCK" blkName))
  (setq blkItr blkDef blkWidth 0.0 blkHeight 0.0)
  (while (setq blkItr (entnext blkItr))
    (if (eq (cdr (assoc 0 (entget blkItr))) "LINE")
      (let* ((e (entget blkItr))
             (p1 (cdr (assoc 10 e)))
             (p2 (cdr (assoc 11 e)))
             (dx (abs (- (car p1) (car p2))))
             (dy (abs (- (cadr p1) (cadr p2)))))
        (if (> dx blkWidth)  (setq blkWidth  dx))
        (if (> dy blkHeight) (setq blkHeight dy))
      )
    )
  )

  ;; ---- 기준선 선택(첫 번째) ----
  (prompt "\n관로와 차도를 선택해주세요..")
  (prompt "\n[1] 관로 선택: ")
  (setq ent1 (car (entsel)))
  (if (not ent1) (progn (prompt "\n❌ 관로 선택 취소.") (exit)))
  (setq obj1 (vlax-ename->vla-object ent1))

  ;; ---- 대상선 선택(두 번째) ----
  (prompt "\n[2] 차도 선택: ")
  (setq ent2 (car (entsel)))
  (if (not ent2) (progn (prompt "\n❌ 차도 선택 취소.") (exit)))
  (setq obj2 (vlax-ename->vla-object ent2))

  ;; ---- 간격 입력 ----
  (prompt (strcat "\n이전 간격값: " (rtos lastInterval 2 2)))
  (initget 6)
  (setq interval (getreal (strcat "\n삽입 간격 (기본값 " (rtos lastInterval 2 2) "): ")))
  (if (not interval) (setq interval lastInterval))
  (setq lastInterval interval)

  ;; ---- 오프셋 ----
  (setq offset (if (= mode "2") (/ interval 2.0) 0.0))

  ;; ---- 기준선 총거리 ----
  (setq dist (vlax-curve-getDistAtParam obj1 (vlax-curve-getEndParam obj1)))
  (setq i 0)

  ;; ---- 루프 ----
  (while (< (+ offset (* i interval)) dist)

    ;; 기준선 점/접선/법선(수직) 벡터
    (setq param (vlax-curve-getParamAtDist obj1 (+ offset (* i interval))))
    (setq pt1   (vlax-curve-getPointAtParam obj1 param))
    (setq tdir  (vlax-curve-getFirstDeriv obj1 param))
    (setq vdir  (list (- (cadr tdir)) (car tdir) 0.0)) ;; 기준선에 '수직' 방향

    ;; 기준선 수직선(양방향 긴 선분) 생성 → 대상선과 교차 계산
    (setq linept1 (mapcar '+ pt1 (mapcar (function (lambda (x) (* x 10000))) vdir)))
    (setq linept2 (mapcar '+ pt1 (mapcar (function (lambda (x) (* x -10000))) vdir)))
    (setq tempEnt (entmakex (list (cons 0 "LINE") (cons 10 linept1) (cons 11 linept2))))

    (setq interraw (vlax-invoke (vlax-ename->vla-object tempEnt) 'IntersectWith obj2 acExtendNone))
    (if tempEnt (entdel tempEnt))

    ;; 교차가 없으면 스킵 (가까운점 대체 사용 안 함)
    (setq interlist (_chunk3 (_to-list interraw)))
    (setq insertPt (if interlist (_closest pt1 interlist) nil))

    (if insertPt
      (progn
        ;; 연결선 생성 (기준선 점 → 교차점) : 수직 방향 보장
        (entmakex
          (list (cons 0 "LINE")
                (cons 8 "PI_Inter1")
                (cons 10 pt1)
                (cons 11 insertPt)))

        ;; 각도 계산(원래 방식 유지)
        (setq angle (+ (get-angle pt1 insertPt) (/ pi 2)))

        ;; 방향 판단/보정(원래 방식 유지)
        (setq dx (abs (- (car insertPt) (car pt1))))
        (setq dy (abs (- (cadr insertPt) (cadr pt1))))

        (if (> dy dx)
          (setq adjPt insertPt) ; 수직방향
          (progn
            (setq isLeft (< (car insertPt) (car pt1)))
            (setq adjX (if isLeft (/ blkWidth 2.0) (* -1 (/ blkWidth 2.0))))
            (setq adjY 0)
            (setq adjPt (list
                          (+ (nth 0 insertPt) (* adjX (cos angle)) (* adjY (- (sin angle))))
                          (+ (nth 1 insertPt) (* adjX (sin angle)) (* adjY (cos angle)))
                          0.0))
          )
        )

        ;; 블럭 삽입(원래 방식 유지)
        (setq blkRef
          (entmakex
            (list
              (cons 0 "INSERT")
              (cons 2 blkName)
              (cons 8 "MN_Street Inter1")
              (cons 10 adjPt)
              (cons 50 angle)
              (cons 41 1.0) (cons 42 1.0) (cons 43 1.0)
            )
          )
        )
      )
      (prompt "\n🔎 기준선 수직선이 대상선과 교차하지 않음 → 이 위치 건너뜀")
    )

    (setq i (1+ i))
  )

  (prompt "\n✅ 완료: 기준선 수직선이 실제로 맞닿는 위치에만 생성했습니다.")
  (princ)
)

;;; MakeEPANET_v0.3.lsp
;;; EPANET INP File Auto Generation Script
;;; Created: 2024-03-21
;;; Modified: 2025-07-15

(vl-load-com)

(defun c:MakeEPANET (/ ss i ent obj coords bulges pts
                        segment-count nodeList coordList all-pipes pipeID nodeID f filename
                        junction-ss fh-ss l_junction_heights l_fh_heights l_pipes_heights l_diameter_heights)

  ;; 사용자 정의 함수: butlast 대체
  (defun my-butlast (lst)
    (reverse (cdr (reverse lst))))

  ;; 고유 파일명 생성 함수
  (defun get-unique-filename (base-name / n full-path)
    (setq n 0)
    (setq full-path base-name)
    (while (findfile full-path)
      (setq n (+ n 1))
      (setq full-path
            (strcat (vl-filename-directory base-name) "\\"
                    (vl-filename-base base-name)
                    "_" (itoa n) ".inp")))
    full-path
  )

  ;; 폰트 사이즈를 임시로 0.001로 변경하는 함수
  (defun change-text-height (layer / ss i ent ent-data height-list)
    (setq height-list '())
    (setq ss (ssget "X" (list '(0 . "TEXT") (cons 8 layer))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq ent (ssname ss i))
          (setq ent-data (entget ent))
          ;; 현재 폰트 높이 저장
          (setq height-list (cons (list ent (cdr (assoc 40 ent-data))) height-list))
          ;; 폰트 높이를 0.001로 변경
          (entmod (subst (cons 40 0.001) (assoc 40 ent-data) ent-data))
          (setq i (1+ i)))
    height-list)

  ;; 폰트 사이즈를 원래대로 복원하는 함수
  (defun restore-text-height (height-list)
    (foreach item height-list
      (setq ent (car item)
            original-height (cadr item)
            ent-data (entget ent))
      ;; 원래 폰트 높이로 복원
      (entmod (subst (cons 40 original-height) (assoc 40 ent-data) ent-data))))

  ;; 특정 좌표 근처의 텍스트 값을 찾는 함수 (더 정확한 검색)
  (defun find-text-at-point (pt layer / ss txt min-dist best-txt i txt-obj txt-pt dist)
    (setq min-dist 999999.0
          best-txt "N/A")
    
    ;; 여러 검색 범위로 시도 (0.5, 1.0, 2.0 단위)
    (foreach search-range '(0.5 1.0 2.0)
      (setq ss (ssget "C" 
                      (list (- (car pt) search-range) (- (cadr pt) search-range)) 
                      (list (+ (car pt) search-range) (+ (cadr pt) search-range)) 
                      (list '(0 . "TEXT") (cons 8 layer))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq txt-obj (ssname ss i)
                  txt-pt (cdr (assoc 10 (entget txt-obj)))
                  dist (distance pt txt-pt))
            ;; 가장 가까운 텍스트 선택
            (if (< dist min-dist)
              (setq min-dist dist
                    best-txt (cdr (assoc 1 (entget txt-obj)))))
            (setq i (1+ i)))
          ;; 충분히 가까운 텍스트를 찾았으면 검색 중단
          (if (< min-dist 0.5)
            (setq search-range 999.0)))))
    best-txt)

  ;; 설정
  (setq nodeList '())     ; ( (pt . N1) (pt . N2) ... )
  (setq coordList '())    ; ( (N1 . pt) (N2 . pt) ... )
  (setq all-pipes '())
  (setq pipeID 1)
  (setq nodeID 1)
  (setq i 0)

  ;; 노드 ID 생성 함수 (L_Junction 레이어의 TEXT 값 사용, 중복 방지)
  (defun get-node-id (pt / found nid junction-text existing-pt)
    ;; 이미 처리된 좌표인지 확인 (정밀도 0.01 단위)
    (setq found nil)
    (foreach existing nodeList
      (if (< (distance pt (car existing)) 0.01)
        (setq found existing)))
    
    (if found
      (cdr found)
      (progn
        ;; L_Junction 레이어에서 TEXT 값 찾기
        (setq junction-text (find-text-at-point pt "L_Junction"))
        (if (= junction-text "N/A")
          (setq nid (strcat "N" (itoa nodeID)))
          (setq nid junction-text))
        (setq nodeList (cons (cons pt nid) nodeList))
        (setq coordList (cons (cons nid pt) coordList))
        (setq nodeID (+ nodeID 1))
        nid)))

  ;; 폰트 사이즈를 임시로 0.001로 변경하여 오류 방지
  (princ "\n📝 폰트 사이즈를 임시로 조정 중...")
  (setq l_junction_heights (change-text-height "L_Junction"))
  (setq l_fh_heights (change-text-height "L_FH"))
  (setq l_pipes_heights (change-text-height "L_Pipes"))
  (setq l_diameter_heights (change-text-height "L_diameter"))
  (princ " 완료")

  ;; 객체 선택
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (not ss)
    (progn (princ "\n❌ 폴리선을 선택하지 않았습니다.") (exit)))

  ;; 각 폴리선 처리
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))

    ;; 좌표 추출
    (setq coords (vlax-get obj 'Coordinates))

    ;; Bulges 속성 체크 후 안전하게 가져오기
    (setq bulges (if (vlax-property-available-p obj 'Bulges)
                   (vlax-get obj 'Bulges)
                   nil))

    ;; 좌표를 점 리스트로 변환
    (setq pts '())
    (repeat (/ (length coords) 2)
      (setq pts (append pts (list (list (car coords) (cadr coords)))))
      (setq coords (cddr coords)))

    ;; 노드 ID 부여
    (setq start (car pts))
    (setq end (last pts))
    (setq nid1 (get-node-id start))
    (setq nid2 (get-node-id end))

    ;; 중간 정점 저장
    (setq mids (cdr (my-butlast pts)))
    (setq pipeIDstr (strcat "P" (itoa pipeID)))
    
    ;; 실제 폴리라인 길이 계산 (곡선 포함)
    (setq actual-length (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
    
    (setq thispipe (list pipeIDstr nid1 nid2 mids actual-length ent))
    (setq all-pipes (append all-pipes (list thispipe)))
    (setq pipeID (+ pipeID 1))
    (setq i (+ i 1)))

  ;; 현재 도면 경로 및 이름
  (setq dwgpath (getvar "DWGPREFIX"))
  (setq dwgname (getvar "DWGNAME"))

  (if (or (not dwgpath) (= dwgpath ""))
    (setq dwgpath "C:\\Temp\\"))
  (if (or (not dwgname) (= dwgname ""))
    (setq dwgname "epanet_output.dwg"))

  ;; 기본 파일 경로
  (setq base-inp-path
        (strcat dwgpath
                (vl-filename-base dwgname)
                ".inp"))

  ;; 유일 파일 경로 생성
  (setq filename (get-unique-filename base-inp-path))

  ;; 파일 열기
  (setq f (open filename "w"))
  (if (not f)
    (progn
      (princ "\n❌ 파일을 열 수 없습니다. 권한 또는 경로를 확인하세요.")
      (exit)))

  ;; [TITLE] - CAD 파일명 사용
  (write-line "[TITLE]" f)
  (write-line (vl-filename-base dwgname) f)
  (write-line "" f)
  
  ;; [OPTIONS] - 기본 옵션값 설정
  (write-line "[OPTIONS]" f)
  (write-line " Units              \tCMD" f)
  (write-line " Headloss           \tH-W" f)
  (write-line " Specific Gravity   \t1" f)
  (write-line " Viscosity          \t1" f)
  (write-line " Trials             \t500" f)
  (write-line " Accuracy           \t0.001" f)
  (write-line " CHECKFREQ          \t2" f)
  (write-line " MAXCHECK           \t10" f)
  (write-line " DAMPLIMIT          \t0" f)
  (write-line " Unbalanced         \tContinue 10" f)
  (write-line " Pattern            \t1" f)
  (write-line " Demand Multiplier  \t1.5" f)
  (write-line " Emitter Exponent   \t0.5" f)
  (write-line " Quality            \tNone mg/L" f)
  (write-line " Diffusivity        \t1" f)
  (write-line " Tolerance          \t0.01" f)
  (write-line "" f)

  ;; [JUNCTIONS] - L_Junction 레이어의 TEXT 값을 ID로, L_FH 레이어의 TEXT 값을 Elev로, Demand를 0으로
  ;; reservoir로 시작하는 ID는 제외
  (write-line "[JUNCTIONS]" f)
  (write-line ";ID              Elev            Demand" f)
  (foreach pair nodeList
    (setq junction-id (cdr pair))
    (setq junction-pt (car pair))
    
    ;; reservoir로 시작하지 않는 경우만 JUNCTIONS에 추가
    (if (not (wcmatch (strcase junction-id) "RESERVOIR*"))
      (progn
        ;; L_FH 레이어에서 고도 값 찾기
        (setq elevation (find-text-at-point junction-pt "L_FH"))
        (if (= elevation "N/A")
          (setq elevation "0"))
        (write-line (strcat junction-id "               "
                           elevation "              "
                           "0") f))))
  
  ;; [RESERVOIRS] - reservoir로 시작하는 ID를 포함
  (write-line "\n[RESERVOIRS]" f)
  (write-line ";ID              Head" f)
  (foreach pair nodeList
    (setq junction-id (cdr pair))
    (setq junction-pt (car pair))
    
    ;; reservoir로 시작하는 경우 RESERVOIRS에 추가
    (if (wcmatch (strcase junction-id) "RESERVOIR*")
      (progn
        ;; L_FH 레이어에서 고도 값 찾기
        (setq elevation (find-text-at-point junction-pt "L_FH"))
        (if (= elevation "N/A")
          (setq elevation "0"))
        (write-line (strcat junction-id "               "
                           elevation) f))))

  ;; [PIPES] - L_Pipes 레이어의 TEXT를 ID로, L_diameter 레이어의 TEXT를 Diameter로, 실제 길이 사용
  (write-line "\n[PIPES]" f)
  (write-line ";ID              Node1           Node2           Length          Diameter        Roughness       MinorLoss       Status" f)
  
  ;; PIPES 데이터 수집
  (setq pipes-data '())
  (foreach p all-pipes
    (setq pipe-id (car p))
    (setq node1 (cadr p))
    (setq node2 (caddr p))
    
    ;; 파이프 중간점 계산 (시작점과 끝점의 중간)
    (setq start-pt (assoc node1 coordList))
    (setq end-pt (assoc node2 coordList))
    (if (and start-pt end-pt)
      (progn
        (setq start-coord (cdr start-pt))
        (setq end-coord (cdr end-pt))
        (setq mid-pt (list (/ (+ (car start-coord) (car end-coord)) 2.0)
                           (/ (+ (cadr start-coord) (cadr end-coord)) 2.0)))
        
        ;; L_Pipes 레이어에서 파이프 ID 찾기 (중간점 위쪽)
        (setq pipe-id-text (find-text-at-point mid-pt "L_Pipes"))
        (if (not (= pipe-id-text "N/A"))
          (setq pipe-id pipe-id-text))
        
        ;; L_diameter 레이어에서 직경 찾기 (중간점 아래쪽)
        (setq diameter-text (find-text-at-point mid-pt "L_diameter"))
        (if (= diameter-text "N/A")
          (setq diameter-text "100"))
        
        ;; 저장된 실제 폴리라인 길이 사용 (곡선 포함)
        (setq pipe-length (nth 4 p)) ; all-pipes에서 저장된 실제 길이
        
        ;; 길이가 0인 경우 방지
        (if (< pipe-length 0.001)
          (setq pipe-length 0.001))
        
        (setq pipes-data (cons (list pipe-id node1 node2 (rtos pipe-length 2 2) diameter-text "110" "0" "Open") pipes-data)))
      ;; 좌표를 찾을 수 없는 경우 기본값 사용
      (setq pipes-data (cons (list pipe-id node1 node2 "100" "100" "110" "0" "Open") pipes-data))))
  
  ;; 이름순 정렬
  (setq pipes-data (vl-sort pipes-data '(lambda (a b) (< (strcase (car a)) (strcase (car b)))))
  
  ;; 정렬된 데이터로 파일 작성
  (foreach pipe pipes-data
    (write-line (strcat (car pipe) "               "
                       (cadr pipe) "              "
                       (caddr pipe) "              "
                       (cadddr pipe) "              "
                       (nth 4 pipe) "              "
                       (nth 5 pipe) "              "
                       (nth 6 pipe) "               "
                       (nth 7 pipe)) f))

  ;; [VERTICES]
  (write-line "\n[VERTICES]" f)
  (foreach p all-pipes
    (setq pipe-id (car p))
    (setq node1 (cadr p))
    (setq node2 (caddr p))
    
    ;; 파이프 중간점 계산 (시작점과 끝점의 중간)
    (setq start-pt (assoc node1 coordList))
    (setq end-pt (assoc node2 coordList))
    (if (and start-pt end-pt)
      (progn
        (setq start-coord (cdr start-pt))
        (setq end-coord (cdr end-pt))
        (setq mid-pt (list (/ (+ (car start-coord) (car end-coord)) 2.0)
                           (/ (+ (cadr start-coord) (cadr end-coord)) 2.0)))
        
        ;; L_Pipes 레이어에서 파이프 ID 찾기 (중간점 위쪽)
        (setq pipe-id-text (find-text-at-point mid-pt "L_Pipes"))
        (if (not (= pipe-id-text "N/A"))
          (setq pipe-id pipe-id-text))))
    
    ;; 중간 정점들에 대해 VERTICES 작성
    (foreach v (cadddr p) ; cadddr는 4번째 요소 (mids)
      (write-line (strcat pipe-id " "
                          (rtos (car v) 2 2) " "
                          (rtos (cadr v) 2 2)) f)))

  ;; [COORDINATES]
  (write-line "\n[COORDINATES]" f)
  (foreach pair coordList
    (write-line (strcat (car pair) " "
                        (rtos (car (cdr pair)) 2 2) " "
                        (rtos (cadr (cdr pair)) 2 2)) f))

  ;; [END]
  (write-line "\n[END]" f)
  (close f)

  ;; 폰트 사이즈를 원래대로 복원
  (princ "\n📝 폰트 사이즈를 원래대로 복원 중...")
  (restore-text-height l_junction_heights)
  (restore-text-height l_fh_heights)
  (restore-text-height l_pipes_heights)
  (restore-text-height l_diameter_heights)
  (princ " 완료")

  (princ (strcat "\n✅ EPANET .inp 파일 생성 완료: " filename))
  (princ)
)

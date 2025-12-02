(defun c:MakeEPANET (/ ss i ent obj coords bulges pts
                        segment-count nodeList coordList all-pipes pipeID nodeID f filename
                        junction-ss fh-ss)

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

  ;; 특정 좌표 근처의 텍스트 값을 찾는 함수 (1 단위 이내)
  (defun find-text-at-point (pt layer / ss txt)
    (setq ss (ssget "C" 
                    (list (- (car pt) 1.0) (- (cadr pt) 1.0)) 
                    (list (+ (car pt) 1.0) (+ (cadr pt) 1.0)) 
                    (list '(0 . "TEXT") (cons 8 layer))))
    (if ss
      (setq txt (cdr (assoc 1 (entget (ssname ss 0)))))
      (setq txt "N/A"))
    txt)

  ;; 설정
  (setq nodeList '())     ; ( (pt . N1) (pt . N2) ... )
  (setq coordList '())    ; ( (N1 . pt) (N2 . pt) ... )
  (setq all-pipes '())
  (setq pipeID 1)
  (setq nodeID 1)
  (setq i 0)

  ;; 노드 ID 생성 함수 (L_Junction 레이어의 TEXT 값 사용)
  (defun get-node-id (pt / found nid junction-text)
    (setq found (assoc pt nodeList))
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
    (setq thispipe (list pipeIDstr nid1 nid2 mids))
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

  ;; [JUNCTIONS] - L_Junction 레이어의 TEXT 값을 ID로, L_FH 레이어의 TEXT 값을 Elev로, Demand를 0으로
  (write-line "[JUNCTIONS]" f)
  (write-line ";ID              Elev            Demand" f)
  (foreach pair nodeList
    (setq junction-id (cdr pair))
    (setq junction-pt (car pair))
    ;; L_FH 레이어에서 고도 값 찾기
    (setq elevation (find-text-at-point junction-pt "L_FH"))
    (if (= elevation "N/A")
      (setq elevation "0"))
    (write-line (strcat junction-id "               "
                           elevation "              "
                           "0") f))

  ;; [PIPES]
  (write-line "\n[PIPES]" f)
  (foreach p all-pipes
    (write-line (strcat (car p) " " (cadr p) " " (caddr p) " 100 12 100") f))

  ;; [VERTICES]
  (write-line "\n[VERTICES]" f)
  (foreach p all-pipes
    (foreach v (cadddr p)
      (write-line (strcat (car p) " "
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

  (princ (strcat "\n✅ EPANET .inp 파일 생성 완료: " filename))
  (princ)
)

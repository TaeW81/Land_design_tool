;;; test_simple.lsp
;;; Simple test file

(defun c:TestSimple ()
  (princ "\nHello World!")
  (princ))

(defun c:TestAdd ()
  (setq a 5)
  (setq b 3)
  (setq result (+ a b))
  (princ (strcat "\nResult: " (itoa result)))
  (princ))

;;; MakeEPANET function
(vl-load-com)

(defun c:MakeEPANET ()
  (setq nodeList '())
  (setq coordList '())
  (setq all-pipes '())
  (setq pipeID 1)
  (setq nodeID 1)
  (setq i 0)

  ;; butlast 대체 함수
  (defun my-butlast (lst)
    (reverse (cdr (reverse lst))))

  ;; 고유 파일명 생성
  (defun get-unique-filename (base-name / n full-path)
    (setq n 0)
    (setq full-path base-name)
    (while (findfile full-path)
      (setq n (+ n 1))
      (setq full-path
            (strcat (vl-filename-directory base-name) "\\"
                    (vl-filename-base base-name)
                    "_" (itoa n) ".inp")))
    full-path)

  ;; 텍스트 찾기 함수
  (defun find-text-at-point (pt layer / ss txt min-dist best-txt i txt-obj txt-pt dist)
    (setq min-dist 999999.0)
    (setq best-txt "N/A")
    
    (foreach search-range '(0.5 1.0 2.0)
      (setq ss (ssget "C" 
                      (list (- (car pt) search-range) (- (cadr pt) search-range)) 
                      (list (+ (car pt) search-range) (+ (cadr pt) search-range)) 
                      (list '(0 . "TEXT") (cons 8 layer))))
      (if ss
        (progn
          (setq i 0)
          (repeat (sslength ss)
            (setq txt-obj (ssname ss i))
            (setq txt-pt (cdr (assoc 10 (entget txt-obj))))
            (setq dist (distance pt txt-pt))
            (if (< dist min-dist)
              (setq min-dist dist
                    best-txt (cdr (assoc 1 (entget txt-obj)))))
            (setq i (+ i 1)))
          (if (< min-dist 0.5)
            (setq search-range 999.0)))))
    best-txt)

  ;; 노드 ID 생성
  (defun get-node-id (pt / found nid junction-text)
    (setq found nil)
    (foreach existing nodeList
      (if (< (distance pt (car existing)) 0.01)
        (setq found existing)))
    
    (if found
      (cdr found)
      (progn
        (setq junction-text (find-text-at-point pt "L_Junction"))
        (if (= junction-text "N/A")
          (setq nid (strcat "N" (itoa nodeID)))
          (setq nid junction-text))
        (setq nodeList (cons (cons pt nid) nodeList))
        (setq coordList (cons (cons nid pt) coordList))
        (setq nodeID (+ nodeID 1))
        nid)))

  ;; 폴리선 선택
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (not ss)
    (progn (princ "\n폴리선을 선택하지 않았습니다.") (exit)))

  ;; 폴리선 처리
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq coords (vlax-get obj 'Coordinates))
    
    (setq pts '())
    (repeat (/ (length coords) 2)
      (setq pts (append pts (list (list (car coords) (cadr coords)))))
      (setq coords (cddr coords)))

    (setq start (car pts))
    (setq end (last pts))
    (setq nid1 (get-node-id start))
    (setq nid2 (get-node-id end))
    (setq mids (cdr (my-butlast pts)))
    (setq pipeIDstr (strcat "P" (itoa pipeID)))
    (setq actual-length (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
    
    (setq thispipe (list pipeIDstr nid1 nid2 mids actual-length ent))
    (setq all-pipes (append all-pipes (list thispipe)))
    (setq pipeID (+ pipeID 1))
    (setq i (+ i 1)))

  ;; 파일 경로 설정
  (setq dwgpath (getvar "DWGPREFIX"))
  (setq dwgname (getvar "DWGNAME"))
  (if (or (not dwgpath) (= dwgpath ""))
    (setq dwgpath "C:\\Temp\\"))
  (if (or (not dwgname) (= dwgname ""))
    (setq dwgname "epanet_output.dwg"))

  (setq base-inp-path (strcat dwgpath (vl-filename-base dwgname) ".inp"))
  (setq filename (get-unique-filename base-inp-path))

  ;; 파일 열기
  (setq f (open filename "w"))
  (if (not f)
    (progn (princ "\n파일을 열 수 없습니다.") (exit)))

  ;; [TITLE]
  (write-line "[TITLE]" f)
  (write-line (vl-filename-base dwgname) f)
  (write-line "" f)
  
  ;; [OPTIONS]
  (write-line "[OPTIONS]" f)
  (write-line " Units              CMD" f)
  (write-line " Headloss           H-W" f)
  (write-line " Specific Gravity   1" f)
  (write-line " Viscosity          1" f)
  (write-line " Trials             500" f)
  (write-line " Accuracy           0.001" f)
  (write-line " CHECKFREQ          2" f)
  (write-line " MAXCHECK           10" f)
  (write-line " DAMPLIMIT          0" f)
  (write-line " Unbalanced         Continue 10" f)
  (write-line " Pattern            1" f)
  (write-line " Demand Multiplier  1.5" f)
  (write-line " Emitter Exponent   0.5" f)
  (write-line " Quality            None mg/L" f)
  (write-line " Diffusivity        1" f)
  (write-line " Tolerance          0.01" f)
  (write-line "" f)

  ;; [JUNCTIONS] - 이름순 정렬
  (write-line "[JUNCTIONS]" f)
  (write-line ";ID              Elev            Demand" f)
  
  (setq junctions-data '())
  (foreach pair nodeList
    (setq junction-id (cdr pair))
    (setq junction-pt (car pair))
    
    (if (not (wcmatch (strcase junction-id) "RESERVOIR*"))
      (progn
        (setq elevation (find-text-at-point junction-pt "L_FH"))
        (if (= elevation "N/A")
          (setq elevation "0"))
        (setq junctions-data (cons (list junction-id elevation "0") junctions-data)))))
  
  (setq junctions-data (vl-sort junctions-data '(lambda (a b) (< (strcase (car a)) (strcase (car b)))))
  (foreach junction junctions-data
    (write-line (strcat (car junction) "               "
                           (cadr junction) "              "
                           (caddr junction)) f))
  
  ;; [RESERVOIRS] - 이름순 정렬
  (write-line "" f)
  (write-line "[RESERVOIRS]" f)
  (write-line ";ID              Head" f)
  
  (setq reservoirs-data '())
  (foreach pair nodeList
    (setq junction-id (cdr pair))
    (setq junction-pt (car pair))
    
    (if (wcmatch (strcase junction-id) "RESERVOIR*")
      (progn
        (setq elevation (find-text-at-point junction-pt "L_FH"))
        (if (= elevation "N/A")
          (setq elevation "0"))
        (setq reservoirs-data (cons (list junction-id elevation) reservoirs-data)))))
  
  (setq reservoirs-data (vl-sort reservoirs-data '(lambda (a b) (< (strcase (car a)) (strcase (car b)))))
  (foreach reservoir reservoirs-data
    (write-line (strcat (car reservoir) "               "
                           (cadr reservoir)) f))

  ;; [PIPES] - 이름순 정렬
  (write-line "" f)
  (write-line "[PIPES]" f)
  (write-line ";ID              Node1           Node2           Length          Diameter        Roughness       MinorLoss       Status" f)
  
  (setq pipes-data '())
  (foreach p all-pipes
    (setq pipe-id (car p))
    (setq node1 (cadr p))
    (setq node2 (caddr p))
    
    (setq start-pt (assoc node1 coordList))
    (setq end-pt (assoc node2 coordList))
    (if (and start-pt end-pt)
      (progn
        (setq start-coord (cdr start-pt))
        (setq end-coord (cdr end-pt))
        (setq mid-pt (list (/ (+ (car start-coord) (car end-coord)) 2.0)
                           (/ (+ (cadr start-coord) (cadr end-coord)) 2.0)))
        
        (setq pipe-id-text (find-text-at-point mid-pt "L_Pipes"))
        (if (not (= pipe-id-text "N/A"))
          (setq pipe-id pipe-id-text))
        
        (setq diameter-text (find-text-at-point mid-pt "L_diameter"))
        (if (= diameter-text "N/A")
          (setq diameter-text "100"))
        
        (setq pipe-length (nth 4 p))
        (if (< pipe-length 0.001)
          (setq pipe-length 0.001))
        
        (setq pipes-data (cons (list pipe-id node1 node2 (rtos pipe-length 2 2) diameter-text "110" "0" "Open") pipes-data)))
      (setq pipes-data (cons (list pipe-id node1 node2 "100" "100" "110" "0" "Open") pipes-data))))
  
  (setq pipes-data (vl-sort pipes-data '(lambda (a b) (< (strcase (car a)) (strcase (car b)))))
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
  (write-line "" f)
  (write-line "[VERTICES]" f)
  (foreach p all-pipes
    (setq pipe-id (car p))
    (setq node1 (cadr p))
    (setq node2 (caddr p))
    
    (setq start-pt (assoc node1 coordList))
    (setq end-pt (assoc node2 coordList))
    (if (and start-pt end-pt)
      (progn
        (setq start-coord (cdr start-pt))
        (setq end-coord (cdr end-pt))
        (setq mid-pt (list (/ (+ (car start-coord) (car end-coord)) 2.0)
                           (/ (+ (cadr start-coord) (cadr end-coord)) 2.0)))
        
        (setq pipe-id-text (find-text-at-point mid-pt "L_Pipes"))
        (if (not (= pipe-id-text "N/A"))
          (setq pipe-id pipe-id-text))))
    
    (foreach v (cadddr p)
      (write-line (strcat pipe-id " "
                          (rtos (car v) 2 2) " "
                          (rtos (cadr v) 2 2)) f)))

  ;; [COORDINATES]
  (write-line "" f)
  (write-line "[COORDINATES]" f)
  (foreach pair coordList
    (write-line (strcat (car pair) " "
                        (rtos (car (cdr pair)) 2 2) " "
                        (rtos (cadr (cdr pair)) 2 2)) f))

  ;; [END]
  (write-line "" f)
  (write-line "[END]" f)
  (close f)

  (princ (strcat "\nEPANET .inp 파일 생성 완료: " filename))
  (princ))

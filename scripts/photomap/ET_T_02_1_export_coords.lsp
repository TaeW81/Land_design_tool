;;; export_coords_v2.2.lsp
(defun c:EXPCOORD (/ file ss sslen i ent obj entType color thickness coords cnt x y this-file this-dir out-file)
  (vl-load-com)
  ;; 1) 현재 LISP 파일(full path) 찾기
  ;;    ? APPLOAD된 파일 이름이 "2_export_coords_v2.2.lsp"라고 가정
  (setq this-file (findfile "2_export_coords_v2.2.lsp"))
  ;; 2) 찾았으면 디렉터리, 못 찾았으면 도면 폴더 사용
  (setq this-dir
        (if this-file
            (vl-filename-directory this-file)
            (getvar "DWGPREFIX")))
  ;; 3) 저장할 파일명 및 경로 설정
  (setq out-file (strcat this-dir "\\temp_selected_coords.txt"))
  ;; 4) 파일 오픈
  (setq file (open out-file "w"))

  (princ "\nEXPCOORD 실행: LINE/LWPOLYLINE/POLYLINE 선택하세요.")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))

  (if (not ss)
      (princ "\n선택 취소됨.")
    (progn
      (setq sslen (sslength ss) i 0)
      (while (< i sslen)
        (setq ent       (ssname ss i)
              obj       (vlax-ename->vla-object ent)
              entType   (vla-get-ObjectName obj)
              color     (vlax-get obj 'Color)
              thickness (vlax-get obj 'Thickness))
        (cond
         ;; 다각형 계열 (LWPOLYLINE, POLYLINE)
         ((vlax-property-available-p obj 'NumberOfVertices)
          (setq cnt (vla-get-NumberOfVertices obj) j 0)
          (while (< j cnt)
            (setq coords (vlax-get obj 'Coordinates)
                  x      (nth j coords)
                  y      (nth (1+ j) coords))
            (write-line
             (strcat
              (rtos x 2 6) "," (rtos y 2 6) ","
              (itoa color) "," (rtos thickness 2 2))
             file)
            (setq j (+ j 2)))
          (write-line "" file))
         ;; 단선 (LINE)
         ((equal entType "AcDbLine")
          (setq sp (vlax-get obj 'StartPoint)
                ep (vlax-get obj 'EndPoint))
          (foreach pt (list sp ep)
            (setq x (car pt) y (cadr pt))
            (write-line
             (strcat
              (rtos x 2 6) "," (rtos y 2 6) ","
              (itoa color) "," (rtos thickness 2 2))
             file))
          (write-line "" file))
         (T
          (princ (strcat "\n처리되지 않은 타입: " entType))))
        (setq i (1+ i)))
      ;; 완료 메시지
      (princ (strcat "\n완료: " out-file " 저장됨"))))

  (close file)
  (princ))
(princ)

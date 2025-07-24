;;; export_coords_v2.2.lsp
(defun c:EXPCOORD (/ file ss sslen i ent obj entType color thickness coords cnt x y this-file this-dir out-file)
  (vl-load-com)
  ;; 1) ���� LISP ����(full path) ã��
  ;;    ? APPLOAD�� ���� �̸��� "2_export_coords_v2.2.lsp"��� ����
  (setq this-file (findfile "2_export_coords_v2.2.lsp"))
  ;; 2) ã������ ���͸�, �� ã������ ���� ���� ���
  (setq this-dir
        (if this-file
            (vl-filename-directory this-file)
            (getvar "DWGPREFIX")))
  ;; 3) ������ ���ϸ� �� ��� ����
  (setq out-file (strcat this-dir "\\temp_selected_coords.txt"))
  ;; 4) ���� ����
  (setq file (open out-file "w"))

  (princ "\nEXPCOORD ����: LINE/LWPOLYLINE/POLYLINE �����ϼ���.")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))

  (if (not ss)
      (princ "\n���� ��ҵ�.")
    (progn
      (setq sslen (sslength ss) i 0)
      (while (< i sslen)
        (setq ent       (ssname ss i)
              obj       (vlax-ename->vla-object ent)
              entType   (vla-get-ObjectName obj)
              color     (vlax-get obj 'Color)
              thickness (vlax-get obj 'Thickness))
        (cond
         ;; �ٰ��� �迭 (LWPOLYLINE, POLYLINE)
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
         ;; �ܼ� (LINE)
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
          (princ (strcat "\nó������ ���� Ÿ��: " entType))))
        (setq i (1+ i)))
      ;; �Ϸ� �޽���
      (princ (strcat "\n�Ϸ�: " out-file " �����"))))

  (close file)
  (princ))
(princ)

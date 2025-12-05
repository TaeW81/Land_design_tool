import os
import subprocess
import tkinter as tk
from tkinter import ttk, messagebox
from PIL import Image, ImageTk
import time
import ctypes
import comtypes.client as cc

class IntegratedTool:
    def __init__(self, root):
        self.root = root
        self.root.title("단지설계 자동화 Tool")
        self.root.configure(bg="#18191c")
        self.root.resizable(False, False)
        
        # 창 위치 추적을 위한 변수 추가
        self.window_positioned = False
        
        # 항상 위에 표시 옵션을 위한 변수 추가 (기본값: True)
        self.always_on_top = tk.BooleanVar(value=True)
        
        # 창을 기본적으로 topmost 상태로 시작
        self.root.attributes('-topmost', True)

        self.logo_img = None
        self.current_frame = None
        self.style = ttk.Style(root)

        self.setup_style()
        self.main_menu()
        
        # 창 포커스 이벤트 바인딩
        self.root.bind('<FocusIn>', self.on_focus_in)
        self.root.bind('<FocusOut>', self.on_focus_out)
        self.root.bind('<Map>', self.on_map)
        
        # 주기적으로 topmost 상태 확인
        self.check_topmost_status()

    def setup_style(self):
        self.style.theme_use('clam')
        self.style.configure('TButton', font=('맑은 고딕', 11, 'bold'), padding=6, background="#23272e", foreground="#00e6d6", borderwidth=0)
        self.style.map('TButton', background=[('active', '#00e6d6')], foreground=[('active', '#23272e')])

        self.style.configure('Exit.TButton', font=('맑은 고딕', 11, 'bold'), padding=6, background="#552222", foreground="#ff6666", borderwidth=0)
        self.style.map('Exit.TButton', background=[('active', '#ff6666')], foreground=[('active', '#18191c')])

        self.style.configure('Back.TButton', font=('맑은 고딕', 11, 'bold'), padding=6, background="#2e2e2e", foreground="#cccccc", borderwidth=0)
        self.style.map('Back.TButton', background=[('active', '#cccccc')], foreground=[('active', '#18191c')])

        self.style.configure('TLabel', font=('맑은 고딕', 14, 'bold'), background="#18191c", foreground="#e6e6e6")

    def clear_frame(self):
        if self.current_frame:
            self.current_frame.destroy()

    def auto_resize_window(self):
        self.root.update_idletasks()
        w = self.root.winfo_reqwidth()
        h = self.root.winfo_reqheight()
        
        # 최초 실행 시에만 화면 가운데로 위치 조정
        if not self.window_positioned:
            x = (self.root.winfo_screenwidth() - w) // 2
            y = (self.root.winfo_screenheight() - h) // 2
            self.root.geometry(f"{w}x{h}+{x}+{y}")
            self.window_positioned = True
        else:
            # 이미 위치가 설정된 경우 크기만 조정하고 위치는 유지
            current_x = self.root.winfo_x()
            current_y = self.root.winfo_y()
            self.root.geometry(f"{w}x{h}+{current_x}+{current_y}")

    def main_menu(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        try:
            script_dir = os.path.dirname(os.path.abspath(__file__))
            logo_path = os.path.join(script_dir, "data", "kunhwa_logo.png")
            logo_img = Image.open(logo_path).resize((160, 80), Image.Resampling.LANCZOS)
            self.logo_img = ImageTk.PhotoImage(logo_img)
            tk.Label(self.current_frame, image=self.logo_img, bg="#18191c").pack()
        except Exception as e:
            print("로고 로딩 오류:", e)
            tk.Label(self.current_frame, text="(로고 없음)", fg="gray", bg="#18191c").pack()

        tk.Label(self.current_frame, text="단지설계부", font=("맑은 고딕", 13, "bold"), bg="#18191c", fg="#e6e6e6").pack()
        tk.Label(self.current_frame, text="업무자동화 통합 툴", font=("맑은 고딕", 13), bg="#18191c", fg="#e6e6e6").pack(pady=10)

        self.menu_button("토공", self.show_earthwork)
        self.menu_button("하수공", self.show_drainage)
        self.menu_button("상수공", self.show_water_supply)
        self.menu_button("포장공", self.show_pavement)
        self.menu_button("기타", self.show_misc)

        # 항상 위에 표시 체크박스 추가
        topmost_frame = tk.Frame(self.current_frame, bg="#18191c")
        topmost_frame.pack(fill='x', padx=10, pady=(10, 5))
        
        topmost_checkbox = tk.Checkbutton(
            topmost_frame, 
            text="항상 위에 표시", 
            variable=self.always_on_top,
            command=self.toggle_topmost,
            bg="#18191c", 
            fg="#e6e6e6",
            selectcolor="#23272e",
            activebackground="#18191c",
            activeforeground="#00e6d6",
            font=("맑은 고딕", 10)
        )
        topmost_checkbox.pack(side='left')

        ttk.Button(self.current_frame, text="종료", command=self.root.quit, style='Exit.TButton')\
            .pack(fill='x', padx=10, pady=(20, 5))
        self.auto_resize_window()

    def menu_button(self, text, command):
        ttk.Button(self.current_frame, text=text, command=command).pack(fill='x', padx=10, pady=5)

    def run_lisp(self, lisp_filename, command=None):
        try:
            acad = cc.GetActiveObject("AutoCAD.Application")
            doc = acad.ActiveDocument
            script_dir = os.path.dirname(os.path.abspath(__file__))
            lisp_path = os.path.join(script_dir, "scripts", "sw", lisp_filename)
            lisp_escaped = lisp_path.replace("\\", "\\\\")
            doc.SendCommand(f'(load "{lisp_escaped}")\n')
            if command:
                doc.SendCommand(f'{command}\n')
            self.bring_cad_to_front()
        except Exception as e:
            messagebox.showerror("LISP 실행 오류", str(e), parent=self.root)

    def run_earthwork_lisp(self):
        try:
            acad = cc.GetActiveObject("AutoCAD.Application")
            doc = acad.ActiveDocument
            script_dir = os.path.dirname(os.path.abspath(__file__))
            lisp_path = os.path.join(script_dir, "scripts", "EW", "1_미성토Line계획", "E_unf3d.lsp")
            lisp_escaped = lisp_path.replace("\\", "\\\\")
            doc.SendCommand(f'(load "{lisp_escaped}")\n')
            doc.SendCommand('E_unf3d\n')
            self.bring_cad_to_front()
        except Exception as e:
            messagebox.showerror("LISP 실행 오류", str(e), parent=self.root)

    def run_pavement_lisp(self, subfolder, lisp_filename, command):
        try:
            acad = cc.GetActiveObject("AutoCAD.Application")
            doc = acad.ActiveDocument
            script_dir = os.path.dirname(os.path.abspath(__file__))
            lisp_path = os.path.join(script_dir, "scripts", "RW", subfolder, lisp_filename)
            lisp_escaped = lisp_path.replace("\\", "\\\\")
            doc.SendCommand(f'(load "{lisp_escaped}")\n')
            doc.SendCommand(f'{command}\n')
            self.bring_cad_to_front()
        except Exception as e:
            messagebox.showerror("LISP 실행 오류", str(e), parent=self.root)

    def run_pc_manhole_excel(self):
        try:
            script_dir = os.path.dirname(os.path.abspath(__file__))
            excel_path = os.path.join(
                script_dir,
                "scripts",
                "SW",
                "1.PC맨홀 조합 자동집계",
                "PC맨홀 조합 자동집계_template.xlsm",
            )
            if not os.path.exists(excel_path):
                messagebox.showerror("파일 오류", f"엑셀 파일을 찾을 수 없습니다.\n{excel_path}", parent=self.root)
                return
            os.startfile(excel_path)
        except Exception as e:
            messagebox.showerror("엑셀 실행 오류", str(e), parent=self.root)

    def show_water_supply(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="상수공", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)

        tk.Label(self.current_frame, text="EPANET 수리계산 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(10, 4))
        ttk.Button(self.current_frame, text="1. 수리계산용 CAD template 자동작성").pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="2. 계획고 적용 from CDS data").pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="3. Cad to INP").pack(fill='x', padx=10, pady=3)

        tk.Label(self.current_frame, text="계획평면도 작성 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 4))
        ttk.Button(self.current_frame, text="1. 계획평면도 작성 from inp").pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="2. 계획평면도 Epanet 결과 적용(수압 등)").pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="3. 계획평면도 text 편집").pack(fill='x', padx=10, pady=3)

        tk.Label(self.current_frame, text="격점상세도 작성 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 4))
        ttk.Button(self.current_frame, text="1. 격점상세도 작성").pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="2. 격점상세도 수량표 출력").pack(fill='x', padx=10, pady=3)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def show_misc(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="기타", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)

        tk.Label(self.current_frame, text="PHOTOMAP", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(5, 3))
        ttk.Button(self.current_frame, text="1. 포토맵 작성", command=self.run_photomap_create).pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="2. CAD 오버레이", command=self.run_cad_overlay).pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="3. 웹 게시", command=self.run_photomap_deploy).pack(fill='x', padx=10, pady=2)

        tk.Label(self.current_frame, text="토지대장 자동정리", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 3))
        ttk.Button(self.current_frame, text="1. PROM 활용 토지대장 정리").pack(fill='x', padx=10, pady=2)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def show_drainage(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="하수공", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)
        tk.Label(self.current_frame, text="계획평면도 작성 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(5, 3))

        button_items = [
            ("1. 우수흐름 방향 표시",       "01_P_flow_direction_by_fh.lsp",     "p_flowdir"),
            ("2. 관로 자동입력(by 유역)",  "02_P_pipe_id_length_auto_by_area.lsp", "p_pipeauto"),
            ("3. 관로 방향 화살표 표기",   "03_P_pipe_arrow_display.lsp",       "p_pipearrow"),
            ("4. 맨홀 아이콘 자동 입력",   "04_P_manhole_auto_insert.lsp",      "p_mhinsert"),
            ("5. 맨홀 명 표기",           "05_P_manhole_labeling.lsp",         "p_mhlabel"),
            ("6. 우수받이 자동작성",       "06_P_streetinlet_auto_create.lsp",  "p_stinlet"),
            ("7. 종단면도 Slope 내용 편집", "07_J_long_section_edit.lsp",        "p_profedit"),
            ("8. 종단 정리(도로별)",       "08_J_long_section_sorting.lsp",     "p_profsort"),
            ("9. 도각 자동생성(종,횡갯수)", "09_S_box_auto_create.lsp",          "p_boxauto"),
            ("10. 키맵 자동생성",         "10_S_keymap_auto_create.lsp",       "p_keymapauto"),
        ]

        for label, filename, command in button_items:
            ttk.Button(
                self.current_frame, 
                text=label, 
                command=lambda f=filename, c=command: self.run_lisp(f, c) if c else self.run_lisp(f)
            ).pack(fill='x', padx=10, pady=2)


        tk.Label(self.current_frame, text="수량산출서 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 3))
        ttk.Button(
            self.current_frame,
            text="1. PC맨홀 조합 자동집계",
            command=self.run_pc_manhole_excel
        ).pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="2. 맨홀 입출입수 자동집계").pack(fill='x', padx=10, pady=2)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def show_earthwork(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="토공", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)

        tk.Label(self.current_frame, text="CDS 편집", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(10, 4))
        ttk.Button(self.current_frame, text="1. 미성토 블록 라인자동 생성", command=self.run_earthwork_lisp).pack(fill='x', padx=10, pady=3)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def show_pavement(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="포장공", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)

        tk.Label(self.current_frame, text="계획 평면도 작성", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(10, 4))
        ttk.Button(self.current_frame, text="1. 횡단보도 점자블록 자동작성", command=lambda: self.run_pavement_lisp("01.점자블록", "BFB_clean.lsp", "BFB")).pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="2. 횡단보도 유도블록 자동작성", command=lambda: self.run_pavement_lisp("02.유도블록", "GB_guide_block.lsp", "GB")).pack(fill='x', padx=10, pady=3)
        ttk.Button(self.current_frame, text="3. 횡단보도 볼라드 자동작성", command=lambda: self.run_pavement_lisp("03.볼라드", "BOL_bollard.lsp", "BOL")).pack(fill='x', padx=10, pady=3)

        tk.Label(self.current_frame, text="수량산출서 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 4))
        ttk.Button(self.current_frame, text="1. 포장계획평면도 CAD 조서 출력").pack(fill='x', padx=10, pady=3)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def run_photomap_create(self):
        try:
            script_dir = os.path.dirname(os.path.abspath(__file__))
            script_path = os.path.join(script_dir, "scripts", "photomap", "ET_T_01_photo_map_create.py")
            subprocess.run(["python", script_path], check=True)
            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showinfo("완료", "Photo Map 작성이 완료되었습니다.", parent=self.root)
            self.root.attributes('-topmost', False)
        except Exception as e:
            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showerror("오류", f"Photo Map 실행 중 오류:\n{e}", parent=self.root)
            self.root.attributes('-topmost', False)

    def run_cad_overlay(self):
        try:
            self.run_lisp_export()
            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showinfo("완료", "CAD 좌표 추출 명령이 전송되었습니다.", parent=self.root)
            self.root.attributes('-topmost', False)
            self.bring_cad_to_front()
            script_dir = os.path.dirname(os.path.abspath(__file__))
            overlay_script = os.path.join(script_dir, "scripts", "photomap", "ET_T_02-2_cad_overlay_apply.py")
            subprocess.run(["python", overlay_script], check=True)
            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showinfo("완료", "CAD 오버레이가 완료되었습니다.", parent=self.root)
            self.root.attributes('-topmost', False)
        except Exception as e:
            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showerror("오류", f"CAD 오버레이 실행 중 오류:\n{e}", parent=self.root)
            self.root.attributes('-topmost', False)

    def run_photomap_deploy(self):
        """Land_design_tool\Photomap_file 저장소로 포토맵 HTML을 배포하고 브라우저로 열기"""
        try:
            bat_path = r"T:\Gits\Land_design_tool\scripts\photomap\커밋deploy_photomap_v2.2.bat"
            if not os.path.exists(bat_path):
                messagebox.showerror("파일 오류", f"배치 파일을 찾을 수 없습니다.\n{bat_path}", parent=self.root)
                return

            self.root.attributes('-topmost', False)
            subprocess.run([bat_path], check=True)

            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showinfo("완료", "포토맵 웹 게시가 완료되었습니다.", parent=self.root)
            self.root.attributes('-topmost', False)

            # 웹 브라우저로 GitHub Pages 사이트 자동 열기
            try:
                url = "https://taew81.github.io/Photomap_file/index.html"
                os.startfile(url)
            except Exception:
                # 브라우저 자동 실행 실패는 치명적이지 않으므로 무시
                pass
        except Exception as e:
            self.root.attributes('-topmost', True)
            self.root.update()
            messagebox.showerror("오류", f"웹 게시 실행 중 오류:\n{e}", parent=self.root)
            self.root.attributes('-topmost', False)

    def run_lisp_export(self):
        acad = cc.GetActiveObject("AutoCAD.Application")
        doc = acad.ActiveDocument
        script_dir = os.path.dirname(os.path.abspath(__file__))
        lisp_path = os.path.join(script_dir, "scripts", "photomap", "ET_T_02_1_export_coords.lsp")
        lisp_escaped = lisp_path.replace("\\", "\\\\")
        cmd = f'(load "{lisp_escaped}")\nEXPCOORD\n'
        doc.SendCommand(cmd)
        time.sleep(0.5)

    def bring_cad_to_front(self):
        user32 = ctypes.WinDLL("user32", use_last_error=True)
        SW_MAXIMIZE = 3
        acad = cc.GetActiveObject("AutoCAD.Application")
        hwnd = acad.HWND
        user32.ShowWindow(hwnd, SW_MAXIMIZE)
        user32.SetForegroundWindow(hwnd)
        root = tk._default_root
        if root:
            root.attributes('-topmost', True)
            root.attributes('-topmost', False)

    def toggle_topmost(self):
        """항상 위에 표시 옵션을 토글하는 함수"""
        if self.always_on_top.get():
            self.root.attributes('-topmost', True)
        else:
            self.root.attributes('-topmost', False)
    
    def on_focus_in(self, event):
        """창이 포커스를 받았을 때 호출되는 함수"""
        if self.always_on_top.get():
            self.root.after(100, lambda: self.root.attributes('-topmost', True))
    
    def on_focus_out(self, event):
        """창이 포커스를 잃었을 때 호출되는 함수"""
        if self.always_on_top.get():
            self.root.after(100, lambda: self.root.attributes('-topmost', True))
    
    def on_map(self, event):
        """창이 다시 표시될 때 호출되는 함수"""
        if self.always_on_top.get():
            self.root.after(100, lambda: self.root.attributes('-topmost', True))
    
    def check_topmost_status(self):
        """주기적으로 topmost 상태를 확인하고 필요시 복원하는 함수"""
        if self.always_on_top.get():
            if not self.root.attributes('-topmost'):
                self.root.attributes('-topmost', True)
        
        # 1초마다 상태 확인
        self.root.after(1000, self.check_topmost_status)

if __name__ == '__main__':
    main_root = tk.Tk()
    IntegratedTool(main_root)
    main_root.mainloop()

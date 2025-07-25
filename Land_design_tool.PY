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

        self.logo_img = None
        self.current_frame = None
        self.style = ttk.Style(root)

        self.setup_style()
        self.main_menu()

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
        x = (self.root.winfo_screenwidth() - w) // 2
        y = (self.root.winfo_screenheight() - h) // 2
        self.root.geometry(f"{w}x{h}+{x}+{y}")

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

        self.menu_button("상수공", self.show_water_supply)
        self.menu_button("우수공", self.show_drainage)
        self.menu_button("기타", self.show_misc)

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
        ttk.Button(self.current_frame, text="3. 웹 게시_미완성").pack(fill='x', padx=10, pady=2)

        tk.Label(self.current_frame, text="토지대장 자동정리", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 3))
        ttk.Button(self.current_frame, text="1. PROM 활용 토지대장 정리").pack(fill='x', padx=10, pady=2)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def show_drainage(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="우수공", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)
        tk.Label(self.current_frame, text="계획평면도 작성 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(5, 3))

        button_items = [
            ("1. 우수흐름 방향 표시",       "01_P_flow_direction_by_fh.lsp"),
            ("2. 관로 자동입력(by 유역)",  "02_P_pipe_id_length_auto_by_area.lsp"),
            ("3. 관로 방향 화살표 표기",   "03_P_pipe_arrow_display.lsp"),
            ("4. 맨홀 아이콘 자동 입력",   "04_P_manhole_auto_insert.lsp"),
            ("5. 맨홀 명 표기",           "05_P_manhole_labeling.lsp"),
            ("6. 우수받이 자동작성_미완성", "06_P_streetinlet_auto_create.lsp"),
            ("7. 종단면도 Slope 내용 편집", "07_J_long_section_edit.lsp"),
            ("8. 종단 정리(도로별)",       "08_J_long_section_sorting.lsp"),
            ("9. 도각 자동생성(종,횡갯수)", "09_S_box_auto_create.lsp"),
            ("10. 키맵 자동생성",         "10_S_keymap_auto_create.lsp"),
        ]

        for label, filename in button_items:
            ttk.Button(self.current_frame, text=label, command=lambda f=filename: self.run_lisp(f)).pack(fill='x', padx=10, pady=2)

        tk.Label(self.current_frame, text="수량산출서 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 3))
        ttk.Button(self.current_frame, text="1. PC맨홀 조합 자동집계").pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="2. 맨홀 입출입수 자동집계").pack(fill='x', padx=10, pady=2)

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

if __name__ == '__main__':
    main_root = tk.Tk()
    IntegratedTool(main_root)
    main_root.mainloop()

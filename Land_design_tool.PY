import os
import subprocess
import tkinter as tk
from tkinter import ttk
from PIL import Image, ImageTk

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

        self.style.configure('TButton',
            font=('맑은 고딕', 11, 'bold'),
            padding=6,
            background="#23272e",
            foreground="#00e6d6",
            borderwidth=0
        )
        self.style.map('TButton',
            background=[('active', '#00e6d6')],
            foreground=[('active', '#23272e')]
        )

        self.style.configure('Exit.TButton',
            font=('맑은 고딕', 11, 'bold'),
            padding=6,
            background="#552222",
            foreground="#ff6666",
            borderwidth=0
        )
        self.style.map('Exit.TButton',
            background=[('active', '#ff6666')],
            foreground=[('active', '#18191c')]
        )

        self.style.configure('Back.TButton',
            font=('맑은 고딕', 11, 'bold'),
            padding=6,
            background="#2e2e2e",
            foreground="#cccccc",
            borderwidth=0
        )
        self.style.map('Back.TButton',
            background=[('active', '#cccccc')],
            foreground=[('active', '#18191c')]
        )

        self.style.configure('TLabel',
            font=('맑은 고딕', 14, 'bold'),
            background="#18191c",
            foreground="#e6e6e6"
        )

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

    def submenu_button(self, label):
        return ttk.Button(self.current_frame, text=label)

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

    def show_drainage(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="우수공", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)

        tk.Label(self.current_frame, text="계획평면도 작성 자동화", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(5, 3))
        ttk.Button(self.current_frame, text="1. fh에 따른 우수 흐름 방향 표시").pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="2. 유역 클릭시 관로 번호 및 길이 자동지정").pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="3. 관로 화살표 표기").pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="4. 맨홀 자동 기입").pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="5. 맨홀 명 표기").pack(fill='x', padx=10, pady=2)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

    def run_photomap_create(self):
        try:
            script_dir = os.path.dirname(os.path.abspath(__file__))
            script_path = os.path.join(script_dir, "scripts", "photomap", "ET_T_01_photo_map_create.py")
            subprocess.run(["python", script_path], check=True)
        except Exception as e:
            print("포토맵 실행 오류:", e)

    def show_misc(self):
        self.clear_frame()
        self.current_frame = tk.Frame(self.root, bg="#18191c")
        self.current_frame.pack(padx=10, pady=10)

        tk.Label(self.current_frame, text="기타", font=("맑은 고딕", 15, "bold"), bg="#18191c", fg="white").pack(pady=5)

        tk.Label(self.current_frame, text="PHOTOMAP", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(5, 3))
        ttk.Button(self.current_frame, text="1. 포토맵 작성", command=self.run_photomap_create).pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="2. CAD 오버레이").pack(fill='x', padx=10, pady=2)
        ttk.Button(self.current_frame, text="3. 웹 게시").pack(fill='x', padx=10, pady=2)

        tk.Label(self.current_frame, text="토지대장 자동정리", font=("맑은 고딕", 11, "bold"), fg="#9966ff", bg="#18191c").pack(pady=(15, 3))
        ttk.Button(self.current_frame, text="1. PROM 활용 토지대장 정리").pack(fill='x', padx=10, pady=2)

        ttk.Button(self.current_frame, text="돌아가기", command=self.main_menu, style='Back.TButton')\
            .pack(fill='x', padx=10, pady=(15, 5))
        self.auto_resize_window()

if __name__ == '__main__':
    main_root = tk.Tk()
    IntegratedTool(main_root)
    main_root.mainloop()

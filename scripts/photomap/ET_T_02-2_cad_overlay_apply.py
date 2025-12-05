#!/usr/bin/env python3
"""
add_cad_overlay_v2.2.py

AutoCAD LISP로 생성된 좌표 TXT를 읽어,
선(Line)/폴리선(Polyline)을 선택한 HTML 파일 위에 오버레이합니다.
(생성된 temp_selected_coords.txt를 DWG 파일이 있는 폴더에서 읽어옵니다)
"""
import os
import json
import shutil
import tkinter as tk
from tkinter import filedialog, messagebox
from pyproj import Transformer
import comtypes.client as cc  # AutoCAD COM 연동

# ===== 설정값 =====
EPSG_CODES = {
    'GRS80(현)_서부': 5185, 'GRS80(현)_중부': 5186, 'GRS80(현)_동부': 5187,
    'GRS80(현)_동해': 5188, 'GRS80(현)_UTMK': 5179,
    '베셀_서부': 5173, '베셀_중부': 5174, '베셀_동부': 5176,
    '베셀_동해': 5177, '베셀_제주': 5175,
    'WGS84_WGS84': 4326, 'WGS84_GoogleTM': 3857,
    'GRS80(구)_서부': 5180, 'GRS80(구)_중부': 5181,
    'GRS80(구)_동부': 5183, 'GRS80(구)_동해': 5184, 'GRS80(구)_제주': 5182,
}

COLOR_MAP = {
    1: '#FF0000', 2: '#FFFF00', 3: '#00FF00', 4: '#00FFFF',
    5: '#0000FF', 6: '#FF00FF', 7: '#FFFFFF'
}

class CadOverlayInjector:
    def __init__(self, master):
        self.master = master
        master.title('HTML에 CAD 오버레이 삽입')
        master.configure(bg="#18191c")
        master.attributes('-topmost', True)
        master.lift()

        # 좌표계 선택용 StringVar
        self.epsg_var = tk.StringVar(value='GRS80(현)_중부')

        # 좌표계 그룹 정의
        self.groups = {
            'GRS80(현)': ['서부', '중부', '동부', '동해', 'UTMK'],
            '베셀':    ['서부', '중부', '동부', '동해', '제주'],
            'WGS84':   ['WGS84', 'GoogleTM'],
            'GRS80(구)': ['서부', '중부', '동부', '동해', '제주'],
        }

        # 라디오버튼 프레임 배치
        for row, (grp, zones) in enumerate(self.groups.items()):
            frame = tk.LabelFrame(
                master, 
                text=grp,
                bg="#18191c",
                fg="#9966ff",
                font=("맑은 고딕", 10, "bold")
            )
            frame.grid(row=row, column=0, padx=10, pady=5, sticky='w')
            for col, zone in enumerate(zones):
                key = f'{grp}_{zone}'
                tk.Radiobutton(
                    frame, 
                    text=zone,
                    variable=self.epsg_var, 
                    value=key,
                    bg="#18191c",
                    fg="#e6e6e6",
                    selectcolor="#23272e",
                    activebackground="#18191c",
                    activeforeground="#00e6d6",
                    font=("맑은 고딕", 9)
                ).grid(row=0, column=col, padx=5, sticky='w')

        # 실행 버튼
        tk.Button(
            master,
            text='HTML 선택 후 오버레이',
            command=self.inject,
            bg="#23272e",
            fg="#00e6d6",
            activebackground="#00e6d6",
            activeforeground="#23272e",
            font=("맑은 고딕", 11, "bold"),
            relief="flat",
            padx=10,
            pady=5
        ).grid(row=len(self.groups), column=0, pady=15)

    def inject(self):
        # 1) EPSG 코드 검증
        epsg_key = self.epsg_var.get()
        epsg_code = EPSG_CODES.get(epsg_key)
        if not epsg_code:
            messagebox.showerror('오류', '유효한 좌표계를 선택하세요.')
            return

        # 2) Active DWG 파일의 폴더에서 temp_selected_coords.txt 경로 설정
        try:
            acad = cc.GetActiveObject('AutoCAD.Application')
            doc = acad.ActiveDocument
            dwg_folder = os.path.dirname(doc.FullName)
            txt_path = os.path.join(dwg_folder, 'temp_selected_coords.txt')
        except Exception as e:
            messagebox.showerror('오류', f'AutoCAD 연동 실패:\n{e}')
            return

        if not os.path.exists(txt_path):
            messagebox.showerror('오류', f'좌표 파일이 없습니다:\n{txt_path}')
            return

        # 3) TXT 읽어서 그룹별 좌표 리스트 만들기
        groups = []
        current = []
        with open(txt_path, 'r', encoding='utf-8') as f:
            for line in f:
                s = line.strip()
                if not s:
                    if current:
                        groups.append(current)
                        current = []
                else:
                    parts = s.split(',')
                    if len(parts) >= 4:
                        x, y = float(parts[0]), float(parts[1])
                        color_idx, thickness = int(parts[2]), float(parts[3])
                        current.append((x, y, color_idx, thickness))
            if current:
                groups.append(current)

        if not groups:
            messagebox.showinfo('정보', '좌표 데이터가 없습니다.')
            return

        # 4) 좌표창 숨기고 HTML 선택창 띄우기
        self.master.withdraw()
        html_path = filedialog.askopenfilename(
            title='기존 HTML 선택',
            filetypes=[('HTML', '*.html')]
        )
        if not html_path:
            self.master.deiconify()
            return

        # 5) 좌표 변환 및 중심점 계산
        transformer = Transformer.from_crs(
            f'EPSG:{epsg_code}', 'EPSG:4326', always_xy=True
        )
        all_pts = [(x, y) for grp in groups for (x, y, _, _) in grp]
        lonlat = [transformer.transform(x, y) for x, y in all_pts]
        avg_lon = sum(l for l, _ in lonlat) / len(lonlat)
        avg_lat = sum(v for _, v in lonlat) / len(lonlat)

        # 6) JavaScript 코드 생성
        js_lines = [f"map.setView([{avg_lat}, {avg_lon}], map.getZoom());"]
        
        # CAD 오버레이 레이어 그룹 생성
        js_lines.append("var cadOverlayGroup = L.layerGroup();")
        
        for grp in groups:
            coords = []
            for x, y, _, _ in grp:
                lon, lat = transformer.transform(x, y)
                coords.append([lat, lon])
            color_idx, thickness = grp[0][2], grp[0][3]
            line_color = COLOR_MAP.get(color_idx, '#FF0000')
            line_weight = thickness if thickness > 0 else 2
            
            # CAD 선을 포토맵 위에 표시하도록 설정
            js_lines.append(
                f"L.polyline({json.dumps(coords)},"
                f"{{color:'{line_color}',weight:{line_weight},opacity:0.9,fillOpacity:0.2}})"
                f".addTo(cadOverlayGroup);"
            )
        
        # CAD 오버레이 레이어를 맵에 추가
        js_lines.append("cadOverlayGroup.addTo(map);")
        
        # 기존 레이어 컨트롤에 CAD 오버레이 추가
        js_lines.append("// 기존 레이어 컨트롤에 CAD 오버레이 추가")
        js_lines.append("var baseLayers = {'Esri Satellite': L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {attribution:'Esri'}),")
        js_lines.append("                'VWorld Road': L.tileLayer('http://xdworld.vworld.kr:8080/2d/Base/202002/{z}/{x}/{y}.png', {attribution:'VWorld'})};")
        js_lines.append("var overlays = {'포토맵 마커': markers, 'CAD 오버레이': cadOverlayGroup};")
        js_lines.append("L.control.layers(baseLayers, overlays, {collapsed:false}).addTo(map);")
        
        js_code = '<script>' + ''.join(js_lines) + '</script>'

        # 7) HTML 삽입 및 저장
        with open(html_path, 'r', encoding='utf-8') as f:
            html = f.read()
        if '</body>' in html:
            new_html = html.replace('</body>', js_code + '\n</body>')
        else:
            new_html = html + js_code

        base, ext = os.path.splitext(html_path)
        save_path = f'{base}_overlay{ext}'
        idx = 1
        while os.path.exists(save_path):
            save_path = f'{base}_overlay_{idx}{ext}'
            idx += 1
        with open(save_path, 'w', encoding='utf-8') as f:
            f.write(new_html)

        # 추가 저장 위치: T:\Gits\Land_design_tool\Photomap_file\
        try:
            extra_dir = r'T:\Gits\Land_design_tool\Photomap_file'
            os.makedirs(extra_dir, exist_ok=True)
            extra_path = os.path.join(extra_dir, os.path.basename(save_path))
            shutil.copy2(save_path, extra_path)
        except Exception:
            # 추가 저장 실패는 치명적이지 않으므로 무시
            pass

        # 삭제: 사용한 temp_selected_coords.txt 파일 제거
        try:
            os.remove(txt_path)
        except Exception:
            pass

        # 8) 완료 메시지 후 프로그램 종료
        messagebox.showinfo('완료', f'저장됨: {save_path}')
        self.master.destroy()


if __name__ == '__main__':
    root = tk.Tk()
    root.configure(bg="#18191c")
    root.attributes('-topmost', True)
    root.lift()
    CadOverlayInjector(root)
    root.mainloop()

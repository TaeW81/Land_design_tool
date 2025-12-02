import ezdxf
import numpy as np
import tkinter as tk
from tkinter import filedialog, messagebox
from pathlib import Path


def compute_intersection(p1, p2, p3, p4):
    a1 = p2[1] - p1[1]
    b1 = p1[0] - p2[0]
    c1 = a1 * p1[0] + b1 * p1[1]

    a2 = p4[1] - p3[1]
    b2 = p3[0] - p4[0]
    c2 = a2 * p3[0] + b2 * p3[1]

    det = a1 * b2 - a2 * b1
    if abs(det) < 1e-10:
        return None

    x = (b2 * c1 - b1 * c2) / det
    y = (a1 * c2 - a2 * c1) / det
    return [x, y]


def compute_precise_offset(points, offset_distance, z_offset):
    # 시계 방향 여부 판단
    area = 0
    for i in range(len(points)):
        x1, y1 = points[i][:2]
        x2, y2 = points[(i + 1) % len(points)][:2]
        area += (x2 - x1) * (y2 + y1)
    clockwise = area > 0

    # 오프셋 방향 정정
    if offset_distance > 0:
        offset_distance *= -1 if clockwise else 1
    else:
        offset_distance *= 1 if clockwise else -1

    z_base_offset = [p[2] + z_offset for p in points]  # Z 일괄 이동

    offset_lines = []
    for i in range(len(points)):
        p1 = np.array(points[i])
        p2 = np.array(points[(i + 1) % len(points)])

        v = p2[:2] - p1[:2]
        length = np.linalg.norm(v)
        if length == 0:
            continue

        normal = np.array([-v[1], v[0]]) / length
        offset_vec = offset_distance * normal

        q1 = p1[:2] + offset_vec
        q2 = p2[:2] + offset_vec

        z1 = z_base_offset[i]
        z2 = z_base_offset[(i + 1) % len(points)]

        offset_lines.append((q1, q2, z1, z2))

    offset_points = []
    for i in range(len(offset_lines)):
        a1, a2, z1a, z2a = offset_lines[i - 1]
        b1, b2, z1b, z2b = offset_lines[i]

        pt2d = compute_intersection(a1, a2, b1, b2)
        if pt2d:
            offset_points.append([pt2d[0], pt2d[1], z1b])
        else:
            offset_points.append([b1[0], b1[1], z1b])

    return offset_points


def process_dxf(input_path, output_path, offset_dist, z_offset):
    doc = ezdxf.readfile(input_path)
    msp = doc.modelspace()

    new_doc = ezdxf.new()
    new_msp = new_doc.modelspace()

    for e in msp.query("POLYLINE"):
        if e.is_3d_polyline:
            pts = [v.dxf.location for v in e.vertices]
            pts = [[p.x, p.y, p.z] for p in pts]

            original_poly = new_msp.add_polyline3d(pts)
            original_poly.close(True)

            offset_pts = compute_precise_offset(pts, offset_dist, z_offset)
            if len(offset_pts) >= 2:
                offset_pts.append(offset_pts[0])  # 폐합
                offset_poly = new_msp.add_polyline3d(offset_pts)
                offset_poly.close(True)

    new_doc.saveas(output_path)


def run_gui():
    def select_file():
        file_path.set(filedialog.askopenfilename(filetypes=[("DXF files", "*.dxf")]))

    def run_offset():
        try:
            input_dxf = file_path.get()
            offset_val = float(offset_entry.get())
            z_val = float(z_entry.get())
            output_dxf = Path(input_dxf).with_name(Path(input_dxf).stem + "_offset.dxf")
            process_dxf(input_dxf, str(output_dxf), offset_val, z_val)
            messagebox.showinfo("Success", f"Offset DXF saved to:\n{output_dxf}")
        except Exception as e:
            messagebox.showerror("Error", str(e))

    root = tk.Tk()
    root.title("3D Polyline Offset Tool")

    file_path = tk.StringVar()

    tk.Label(root, text="Select DXF file:").grid(row=0, column=0, padx=5, pady=5)
    tk.Entry(root, textvariable=file_path, width=40).grid(row=0, column=1, padx=5)
    tk.Button(root, text="Browse", command=select_file).grid(row=0, column=2, padx=5)

    tk.Label(root, text="Offset distance (+ = inward):").grid(row=1, column=0, padx=5, pady=5)
    offset_entry = tk.Entry(root)
    offset_entry.grid(row=1, column=1, padx=5)

    tk.Label(root, text="Z height offset (+ = up, - = down):").grid(row=2, column=0, padx=5, pady=5)
    z_entry = tk.Entry(root)
    z_entry.grid(row=2, column=1, padx=5)

    tk.Button(root, text="Run Offset", command=run_offset).grid(row=3, column=1, pady=10)

    root.mainloop()


if __name__ == "__main__":
    try:
        run_gui()
    except Exception as e:
        print("오류 발생:", e)
    input("작업이 완료되었거나 오류가 발생했습니다. Enter 키를 누르면 종료됩니다.")

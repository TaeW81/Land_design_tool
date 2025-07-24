import os
import json
import base64
import io
import sys
import uuid
import shutil  # HTML 복사 기능
from PIL import Image, ExifTags
from PIL.ExifTags import TAGS, GPSTAGS
import tkinter as tk
from tkinter import filedialog, messagebox

# 버전 2.1: v2.0基础 + 현위치 버튼 + HTML 복사功能
AUTHORIZED_MACS = {
    "80-E8-2C-EF-97-E0": "124507",
    "6C-0B-5E-42-EC-0A": "125873",
}

def get_mac_address():
    mac = uuid.getnode()
    return "-".join(format(mac, "012X")[i:i+2] for i in range(0, 12, 2))

if get_mac_address().upper() not in AUTHORIZED_MACS:
    print("해당 장비에서만 실행 가능한 프로그램입니다.")
    sys.exit()

def select_folder():
    root = tk.Tk()
    root.withdraw()
    return filedialog.askdirectory(title="사진 폴더를 선택하세요")

def show_complete_message(path):
    root = tk.Tk()
    root.attributes("-topmost", True)
    root.withdraw()
    messagebox.showinfo("생성 완료", f'"{path}" 생성 완료')
    root.destroy()

def get_exif_gps(path):
    try:
        img = Image.open(path)
        exif = img._getexif()
        if not exif:
            return None
        data = {TAGS.get(k, k): v for k, v in exif.items()}
        gps_info = data.get('GPSInfo')
        return {GPSTAGS.get(k, k): gps_info[k] for k in gps_info} if gps_info else None
    except:
        return None

def dms_to_dd(dms, ref):
    try:
        deg = dms[0].numerator / dms[0].denominator
        mn = dms[1].numerator / dms[1].denominator
        sec = dms[2].numerator / dms[2].denominator
    except:
        deg, mn, sec = map(float, dms)
    dd = deg + mn/60 + sec/3600
    return -dd if ref in ['S','W'] else dd

def get_photo_datetime(path):
    try:
        img = Image.open(path)
        exif = img._getexif() or {}
        ds = exif.get(36867)
        if ds:
            return ds.replace(':','년',1).replace(':','월',1).replace(' ','일 ').replace(':','시',1).replace(':','분') + '초'
    except:
        pass
    return '촬영일자 없음'

def auto_rotate_image(img):
    try:
        exif = img._getexif()
        if exif:
            ori_tag = next(k for k,v in ExifTags.TAGS.items() if v=='Orientation')
            ori = exif.get(ori_tag)
            if ori == 3:
                img = img.rotate(180,expand=True)
            elif ori == 6:
                img = img.rotate(270,expand=True)
            elif ori == 8:
                img = img.rotate(90,expand=True)
    except:
        pass
    return img

def get_thumbnail_base64_and_size(path):
    try:
        img = Image.open(path)
        img = auto_rotate_image(img)
        max_h = 900
        if img.height > max_h:
            ratio = max_h / img.height
            img = img.resize((int(img.width*ratio), max_h), Image.LANCZOS)
        buf = io.BytesIO()
        img.save(buf, format='JPEG')
        return base64.b64encode(buf.getvalue()).decode(), img.width, img.height
    except:
        return None, None, None

def collect_image_files(folder):
    return [os.path.join(r,f) for r,_,fs in os.walk(folder) for f in fs if f.lower().endswith(('.jpg','.jpeg'))]

def get_unique_path(directory, filename):
    base, ext = os.path.splitext(filename)
    cand = filename
    i = 1
    while os.path.exists(os.path.join(directory,cand)):
        cand = f"{base}_{i}{ext}"
        i+=1
    return os.path.join(directory,cand)

def main():
    folder = select_folder()
    if not folder:
        return  # 폴더 선택 안 했을 경우 아무 메시지도 출력하지 않고 종료

    pts = []
    for p in collect_image_files(folder):
        gps = get_exif_gps(p)
        if gps and 'GPSLatitude' in gps and 'GPSLongitude' in gps:
            try:
                lat = dms_to_dd(gps['GPSLatitude'], gps['GPSLatitudeRef'])
                lon = dms_to_dd(gps['GPSLongitude'], gps['GPSLongitudeRef'])
                thumb, _, _ = get_thumbnail_base64_and_size(p)
                taken = get_photo_datetime(p)
                pts.append({'path': os.path.relpath(p, folder), 'lat': lat, 'lon': lon, 'thumb_b64': thumb, 'taken_time': taken})
            except Exception as e:
                print(f"{p} 오류: {e}")


    if not pts:
        root = tk.Tk()
        root.withdraw()
        messagebox.showinfo("GPS 정보 없음", "폴더 내 GPS 좌표가 포함된 사진이 없습니다.")
        root.destroy()
        sys.exit()

    avg_lat = sum(x['lat'] for x in pts)/len(pts)
    avg_lon = sum(x['lon'] for x in pts)/len(pts)
    data_json = json.dumps(pts,ensure_ascii=False)

    esri = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    vworld = "http://xdworld.vworld.kr:8080/2d/Base/202002/{z}/{x}/{y}.png"

    html = f"""
<!DOCTYPE html>
<html lang="ko">
<head>
<meta charset="utf-8">
<title>Photo Map v2.1</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.3/dist/leaflet.css" />
<link rel="stylesheet" href="https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css" />
<link rel="stylesheet" href="https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css" />
<style>
body {{margin:0;display:flex;height:100vh;}}
#map-container {{width:100%;transition:width .3s;}}
#map {{height:100vh;}}
#photo-container {{width:0;overflow-y:auto;padding:10px;box-sizing:border-box;
  display:none;flex-direction:column;align-items:center;transition:width .3s;}}
#photo-container img {{max-width:100%;max-height:85vh;object-fit:contain;}}
h4,p {{margin:4px 0;text-align:center;}}
#loc-btn {{position:absolute;bottom:20px;left:20px;z-index:2000;
  padding:6px 8px;background:#fff;border:1px solid #ccc;border-radius:4px;
  cursor:pointer;box-shadow:0 2px 4px rgba(0,0,0,0.3);}}
</style>
</head>
<body>
<div id="map-container"><div id="map"></div></div>
<button id="loc-btn">현위치</button>
<div id="photo-container"></div>
<script src="https://unpkg.com/leaflet@1.9.3/dist/leaflet.js"></script>
<script src="https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js"></script>
<script>
var baseLayers = {{'Esri Satellite':L.tileLayer('{esri}',{{attribution:'Esri'}}),
                    'VWorld Road':L.tileLayer('{vworld}',{{attribution:'VWorld'}})}};
var map = L.map('map',{{center:[{avg_lat},{avg_lon}],zoom:16,layers:[baseLayers['Esri Satellite']]}});
L.control.layers(baseLayers,null,{{collapsed:false}}).addTo(map);
var markers = L.markerClusterGroup({{maxClusterRadius:40}});
var data = {data_json};
data.forEach(function(p){{
  L.marker([p.lat,p.lon],{{title:p.path}}).on('click',function(){{
    document.getElementById('map-container').style.width='50%';
    var pc=document.getElementById('photo-container');pc.style.display='flex';pc.style.width='50%';
    pc.innerHTML='<h4>'+p.path+'</h4><p>['+p.taken_time+']</p>'+'<img src="data:image/jpeg;base64,'+p.thumb_b64+'"/>';
    setTimeout(function(){{map.invalidateSize();}},300);
  }}).addTo(markers);
}});
map.addLayer(markers);
document.getElementById('photo-container').onclick=function(){{
  this.style.display='none';this.style.width='0';
  document.getElementById('map-container').style.width='100%';
  setTimeout(function(){{map.invalidateSize();}},300);
}};
document.getElementById('loc-btn').onclick=function(){{map.locate({{setView:true,maxZoom:18}});}};
map.on('locationfound',function(e){{
  L.marker(e.latlng).addTo(map).bindPopup(`당신의 위치 (±${{Math.round(e.accuracy)}}m)`).openPopup();
}});
map.on('locationerror',function(e){{alert('위치 정보를 가져올 수 없습니다: '+e.message);}});
</script>
</body>
</html>
    """

    save_name=os.path.basename(os.path.normpath(folder))+'.html'
    out=get_unique_path(folder,save_name)
    with open(out,'w',encoding='utf-8') as f:
        f.write(html)
    os.makedirs(r'C:\photomap',exist_ok=True)
    shutil.copy2(out,os.path.join(r'C:\photomap',os.path.basename(out)))

    show_complete_message(out)

if __name__=='__main__':
    main()

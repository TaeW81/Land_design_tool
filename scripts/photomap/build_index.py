import os


def main():
    # T:\Gits\Land_design_tool\Photomap_file\ 경로로 명시적으로 설정
    target_dir = r"T:\Gits\Land_design_tool\Photomap_file"
    
    if not os.path.exists(target_dir):
        print(f"오류: 디렉토리를 찾을 수 없습니다: {target_dir}")
        return
    
    base_dir = target_dir
    os.chdir(base_dir)

    # 폴더 내의 모든 HTML 파일 찾기 (index.html 제외)
    print(f"디렉토리 검색: {base_dir}")
    all_files = os.listdir(base_dir)
    print(f"전체 파일 수: {len(all_files)}")
    
    html_files = []
    for f in all_files:
        full_path = os.path.join(base_dir, f)
        if os.path.isfile(full_path):
            if f.lower().endswith(".html"):
                if f.lower() != "index.html":
                    html_files.append(f)
                    print(f"  HTML 파일 발견: {f}")
                else:
                    print(f"  index.html 제외: {f}")
            else:
                print(f"  HTML이 아닌 파일: {f}")
        else:
            print(f"  디렉토리/링크: {f}")
    
    print(f"\n총 {len(html_files)}개의 HTML 파일이 발견되었습니다.")
    
    if not html_files:
        print("경고: HTML 파일이 없습니다.")
        # 빈 목록으로도 index.html은 생성

    lines = []
    lines.append("<!DOCTYPE html>")
    lines.append('<html lang="ko">')
    lines.append("<head>")
    lines.append('    <meta charset="UTF-8">')
    lines.append('    <meta name="viewport" content="width=device-width, initial-scale=1.0">')
    lines.append("    <title>Photo Map Index</title>")
    lines.append("    <style>")
    lines.append("        body { font-family: Arial, sans-serif; padding: 2em; }")
    lines.append("        h1 { color: #333; }")
    lines.append("        ul { line-height: 1.8; }")
    lines.append("    </style>")
    lines.append("</head>")
    lines.append("<body>")
    lines.append("    <h1>배포된 지도 링크 목록</h1>")
    lines.append("    <ul>")

    base_url = "http://taew81.github.io/Photomap_file/"
    sorted_files = sorted(html_files)
    
    print(f"\n링크 생성 중...")
    for name in sorted_files:
        # GitHub Pages는 파일명을 그대로 사용
        # 브라우저가 자동으로 URL 인코딩 처리하므로 파일명을 그대로 사용
        url = base_url + name
        lines.append(f'        <li><a href="{url}" target="_blank">{name}</a></li>')
        print(f"  링크 추가: {name} -> {url}")

    lines.append("    </ul>")
    lines.append("</body>")
    lines.append("</html>")

    out_path = os.path.join(base_dir, "index.html")
    
    # 기존 index.html이 있으면 백업 (선택사항)
    if os.path.exists(out_path):
        print(f"기존 index.html 발견, 새로 생성합니다...")
    
    # UTF-8 인코딩으로 새로 작성 (항상 최신 파일 목록 반영)
    with open(out_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    
    print(f"\n{'='*50}")
    print(f"index.html 생성 완료!")
    print(f"총 {len(sorted_files)}개의 HTML 파일 링크 포함")
    print(f"{'='*50}")
    for i, name in enumerate(sorted_files, 1):
        print(f"  {i}. {name}")
    print(f"{'='*50}\n")


if __name__ == "__main__":
    main()



import os


def main():
    base_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(base_dir)

    html_files = [f for f in os.listdir(base_dir)
                  if f.lower().endswith(".html") and f.lower() != "index.html"]

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
    for name in sorted(html_files):
        url = base_url + name
        lines.append(f'        <li><a href="{url}" target="_blank">{name}</a></li>')

    lines.append("    </ul>")
    lines.append("</body>")
    lines.append("</html>")

    out_path = os.path.join(base_dir, "index.html")
    with open(out_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))


if __name__ == "__main__":
    main()



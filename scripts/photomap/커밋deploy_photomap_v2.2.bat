@echo off

REM ===== working directory =====
cd /d T:\Gits\Photomap_file\

echo ===============================
echo Building index.html with Python...
echo ===============================

python build_index.py

echo ===============================
echo Syncing with GitHub...
echo ===============================

git pull --rebase origin main
git add .
git commit -m "auto deploy: html and index.html updated"
git push origin main

echo ===============================
echo Done: http://taew81.github.io/Photomap_file/index.html
echo ===============================



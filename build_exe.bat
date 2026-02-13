@echo off
echo ========================================
echo Land Design Tool EXE 빌드 시작
echo ========================================

REM PyInstaller 설치 확인
python -c "import PyInstaller" 2>nul
if errorlevel 1 (
    echo PyInstaller가 설치되어 있지 않습니다. 설치 중...
    pip install pyinstaller
)

REM requirements.txt의 패키지 설치
echo.
echo 필요한 패키지 설치 중...
pip install -r requirements.txt

echo.
echo ========================================
echo EXE 파일 생성 중...
echo ========================================

REM PyInstaller로 EXE 생성
pyinstaller Land_design_tool_v02.spec

if errorlevel 1 (
    echo.
    echo ========================================
    echo 빌드 실패!
    echo ========================================
    pause
    exit /b 1
)

echo.
echo ========================================
echo 빌드 완료!
echo ========================================
echo.
echo EXE 파일 위치: dist\Land_design_tool_v02.exe
echo.
pause









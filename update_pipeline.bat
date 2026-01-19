@echo off
echo ========================================================
echo   UPDATING PENSIONS PIPELINE - DO NOT CLOSE WINDOW
echo ========================================================

:: 1. Go to the repo location
cd /d C:\Work\Pensions_Pipeline

:: 2. Reset any local changes to scripts (Safety First! Prevents merge conflicts)
::    (Assumption: They should NOT be editing the R scripts, only the CSVs)
git reset --hard origin/main

:: 3. Pull latest code
git pull origin main

:: 4. Check if R packages need installing (Optional optimization)
::    "runner.R" can handle this logic
"C:\Program Files\R\R-4.5.1\bin\Rscript.exe" runner.R setup

echo ========================================================
echo   UPDATE COMPLETE. YOU MAY NOW RUN THE PROCESS.
echo ========================================================
pause

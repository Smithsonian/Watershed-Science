rem Lydia LaGorga
rem sync markdown htmls to Smithsonian GitHub thorough git

@echo off
rem TEST 
rem bat file for mirroring markdowns to GitHub
rem Lydia LaGorga 6-10-2025 
echo on
setlocal

git init

git pull origin main

"C:\Program Files\R\R-4.5.0\bin\Rscript.exe" "C:\Users\LaGorgaL.S\Documents\Watershed-Science\daily_checks\renderweeklycheck.R" --pandoc="C:/Program Files (x86)/pandoc-3.7.0.1"

git add "C:\Users\LaGorgaL.S\Documents\Watershed-Science\daily_checks\anything_WEIRd.html"

git commit -am "auto update"

git push origin main

pause
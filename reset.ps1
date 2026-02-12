# ... (Keep your Get-R-Exe and Get-Rscript-Exe functions) ...

# 1. KILL PROCESSES HARDER
# RStudio wrappers sometimes spawn 'rsession.exe' under different names.
Get-Process | Where-Object { $_.Name -match "Rsession|Rstudio|Rgui" } | Stop-Process -Force -ErrorAction SilentlyContinue

# 2. TARGETED NUKE
# Instead of searching all LibPaths, we force a clean slate in the Primary User Lib
$PrimaryLib = $LibPaths[0]
foreach ($pkg in $PackageOrder) {
  $pkgFolder = Join-Path $PrimaryLib $pkg
  if (Test-Path $pkgFolder) {
    Write-Host "Nuking $pkg from $PrimaryLib..."
    Remove-Item -Path $pkgFolder -Recurse -Force
  }
}

# 3. FAST INSTALL (Skip the Tarball step)
# Since you're on a local machine, 'R CMD INSTALL' can run directly on the folder.
# This is much faster and avoids Temp folder permission issues.
foreach ($pkg in $PackageOrder) {
  $dir = $PackageDirs[$pkg]
  Write-Host "Installing $pkg directly from $dir..."
  & $RExe CMD INSTALL --library="$PrimaryLib" --no-multiarch --with-keep.source "$dir"
  if ($LASTEXITCODE -ne 0) { throw "Installation of $pkg failed." }
}

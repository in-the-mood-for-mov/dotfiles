#!/usr/bin/env pwsh

function Update-Link([string]$SourcePath, [string]$TargetPath) {
  if (Test-Path $TargetPath) {
    $targetFile = Get-Item -Force $TargetPath

    if ($null -eq $targetFile) {
      New-Item -ItemType Directory $(Split-Path $TargetPath) -ErrorAction SilentlyContinue | Out-Null
    } elseif ([bool]($targetFile.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
      Remove-Item -Path $TargetPath -Force
    } else {
      Write-Host -ForegroundColor "Red" ("{0} already exists, skipping." -f $TargetPath)
      return
    }
  }

  Write-Host -ForegroundColor "Green" ("{0} -> {1}" -f $TargetPath, $SourcePath)
  $sourceFullPath = $([IO.Path]::GetFullPath($(Join-Path $PSScriptRoot $SourcePath)))
  New-Item -ItemType "SymbolicLink" -Path $TargetPath -Value $sourceFullPath | Out-Null
}

$emacsFolderName = if ($IsWindows) { ".emacs.d" } else { "emacs" }
$links = [ordered]@{
  "emacs" = Join-Path ([Environment]::GetFolderPath("ApplicationData")) $emacsFolderName;
  "gitconfig" = (Join-Path $HOME ".gitconfig");
  "gitignore" = (Join-Path $HOME ".gitignore");
}

foreach ($pair in $links.GetEnumerator()) {
  $targetPath = $pair.Value
  if ($targetPath) {
    Update-Link -SourcePath $pair.Key -TargetPath $targetPath
  }
}

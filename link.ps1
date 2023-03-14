#!/usr/bin/env pwsh

function Update-Link([string]$SourcePath, [string]$TargetPath) {
  $targetDir = $(Split-Path $TargetPath)
  if (-not (Test-Path $targetDir)) {
    New-Item -ItemType Directory $targetDir -ErrorAction SilentlyContinue | Out-Null
  }

  if (Test-Path $TargetPath) {
    $targetFile = Get-Item -Force $TargetPath

    if ($null -eq $targetFile) {
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
  "profile.ps1" = $Profile.CurrentUserAllHosts;
  "emacs" = Join-Path ([Environment]::GetFolderPath("ApplicationData")) $emacsFolderName;
  "gitconfig" = (Join-Path $HOME ".gitconfig");
  "gitignore" = (Join-Path $HOME ".gitignore");
  "clang-tidy" = (Join-Path $HOME ".clang-tidy");
}

foreach ($pair in $links.GetEnumerator()) {
  $targetPath = $pair.Value
  if ($targetPath) {
    Update-Link -SourcePath $pair.Key -TargetPath $targetPath
  }
}

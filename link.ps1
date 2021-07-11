#!/usr/bin/env pwsh

function Update-Link([string]$SourcePath, [string]$TargetPath) {
  $targetFile = Get-Item -Force $TargetPath

  if ($null -eq $targetFile) {
    New-Item -ItemType Directory $(Split-Path $TargetPath) -ErrorAction SilentlyContinue | Out-Null
  } elseif ([bool]($targetFile.Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    Remove-Item -Path $TargetPath -Force
    $targetFile = $null
  }

  if ($null -eq $targetFile) {
    Write-Host -ForegroundColor "Green" ("{0} -> {1}" -f $TargetPath, $SourcePath)
    $sourceFullPath = $([IO.Path]::GetFullPath($(Join-Path $PSScriptRoot $SourcePath)))
    New-Item -ItemType "SymbolicLink" -Path $TargetPath -Value $sourceFullPath | Out-Null
  } else {
    Write-Host -ForegroundColor "Red" ("{0} already exists, skipping." -f $TargetPath)
  }
}

$applicationData = [Environment]::GetFolderPath([Environment+SpecialFolder]::ApplicationData)

if ($IsWindows) {
  $links = [ordered]@{
    "emacs.d" = (Join-Path $applicationData ".emacs.d");
    "gitconfig" = (Join-Path $HOME ".gitconfig");
    "gitconfig-nt" = (Join-Path $HOME ".gitconfig-platform");
    "gitignore" = (Join-Path $HOME ".gitignore");
  }
} else {
  $links = [ordered]@{
    "fish" = (Join-Path $applicationData "fish");
    "emacs.d" = (Join-Path $HOME ".emacs.d");
    "spacemacs.d" = (Join-Path $HOME ".spacemacs.d");
    "gitconfig" = (Join-Path $HOME ".gitconfig");
    "gitconfig-darwin" = (Join-Path $HOME ".gitconfig-platform");
    "gitignore" = (Join-Path $HOME ".gitignore");
  }
}

foreach ($pair in $links.GetEnumerator()) {
  $targetPath = $pair.Value
  if ($targetPath) {
    Update-Link -SourcePath $pair.Key -TargetPath $targetPath
  }
}

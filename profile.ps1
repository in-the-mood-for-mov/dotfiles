Set-Alias -Name ls -Value Get-ChildItem
$env:LANG = "en_CA.utf-8"
Set-PSReadlineOption -EditMode Emacs -BellStyle Visual
[Console]::TreatControlCAsInput = $True

Invoke-Command {
  $applicationData = [Environment]::GetFolderPath("ApplicationData")

  $env:RUSTUP_HOME = Join-Path $applicationData "rustup"
  $env:CARGO_HOME = Join-Path $applicationData "cargo"
  $env:PATH = "$(Join-Path $env:CARGO_HOME "bin")$([IO.Path]::PathSeparator)$env:PATH"
}

function Prompt {
  param ()

  $currentPath = $ExecutionContext.SessionState.Path.CurrentLocation
  $currentFolder = Split-Path -Leaf -Path $currentPath
  "$($currentFolder)\ 🍕 $('>' * $($NestedPromptLevel))"
}

Import-Module posh-git

if (Get-Command "opam" -ErrorAction SilentlyContinue) {
  foreach ($line in (opam env --shell zsh)) {
    if ($line -match "^(?<var>\w+)='(?<value>.*)'") {
      [Environment]::SetEnvironmentVariable($Matches.var, $Matches.value, "Process")
    }
  }
}

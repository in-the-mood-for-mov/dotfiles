Set-Alias -Name ls -Value exa

Set-Item -Path env:LANG -Value "en_CA.utf-8"

Set-PSReadlineOption -EditMode Emacs -BellStyle Visual
[Console]::TreatControlCAsInput = $True

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

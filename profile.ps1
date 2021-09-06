Set-Alias -Name ls -Value exa.exe

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

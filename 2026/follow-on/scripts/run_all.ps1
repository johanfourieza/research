# Run the complete analysis in clean R processes on Windows.
# Usage from the project root:
#   powershell -NoProfile -ExecutionPolicy Bypass -File scripts/run_all.ps1

# R uses stderr for messages and warnings even when it exits successfully.
# Judge native stages by LASTEXITCODE rather than PowerShell's treatment of any
# stderr text as a NativeCommandError.
$ErrorActionPreference = 'Continue'
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$projectRoot = Split-Path -Parent $scriptDir
$logDir = Join-Path $projectRoot 'output\logs'
New-Item -ItemType Directory -Force -Path $logDir | Out-Null

$rCommand = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($rCommand) {
    $rscript = $rCommand.Source
} else {
    $rscript = Get-ChildItem -LiteralPath (Join-Path $env:ProgramFiles 'R') `
        -Filter Rscript.exe -Recurse -ErrorAction SilentlyContinue |
        Where-Object { $_.FullName -notmatch '\\bin\\x64\\' } |
        Sort-Object FullName -Descending |
        Select-Object -First 1 -ExpandProperty FullName
}
if (-not $rscript) { throw 'Rscript.exe was not found.' }

$steps = @(
    '01_data.R',
    '02_descriptives.R',
    '03_rd.R',
    '04_events.R',
    '05_behavioural.R',
    '06_mechanisms.R',
    '07_robustness.R',
    '08_figures_tables.R',
    '09_release.R'
)

$manifest = [System.Collections.Generic.List[object]]::new()
$pipelineStatus = 0
foreach ($step in $steps) {
    Write-Host "`n===== $step ====="
    $timer = [System.Diagnostics.Stopwatch]::StartNew()
    $stderrLog = Join-Path $logDir (([IO.Path]::GetFileNameWithoutExtension($step)) + '_stderr.txt')
    & $rscript (Join-Path $scriptDir $step) 2> $stderrLog
    $status = $LASTEXITCODE
    $timer.Stop()
    $manifest.Add([pscustomobject]@{
        script = $step
        exit_status = $status
        seconds = [math]::Round($timer.Elapsed.TotalSeconds, 1)
        status = if ($status -eq 0) { 'ok' } else { 'FAILED' }
    })
    Write-Host ("   exit {0}, {1:N1} seconds" -f $status, $timer.Elapsed.TotalSeconds)
    if ($status -ne 0) {
        $pipelineStatus = $status
        Write-Host '   stopping: downstream outputs were not regenerated'
        Write-Host "   stderr: $stderrLog"
        break
    }
}

$manifestPath = Join-Path $logDir 'run_manifest.csv'
$manifest | Export-Csv -LiteralPath $manifestPath -NoTypeInformation
$manifest | Format-Table -AutoSize
Write-Host ("{0} of {1} stages completed successfully" -f `
    ($manifest | Where-Object exit_status -eq 0).Count, $manifest.Count)
exit $pipelineStatus

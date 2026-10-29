# Build the fui demo with gfortran (default) or Intel ifx.
#
#   .\build.ps1
#   .\build.ps1 -Compiler ifx -Run
#   .\build.ps1 -Run

param(
    [ValidateSet('gfortran', 'ifx')]
    [string] $Compiler = 'gfortran',
    [switch] $Run
)

$ErrorActionPreference = 'Stop'
Set-Location -Path $PSScriptRoot

$sources = @(
    'src/core/ui_kinds.f90'
    'src/core/ui_color_hct.f90'
    'src/core/ui_theme.f90'
    'src/core/ui_anim.f90'
    'src/core/ui_font.f90'
    'src/core/ui_draw.f90'
    'src/core/ui_input.f90'
    'src/core/ui.f90'
    'src/core/ui_global.f90'
    'src/core/ui_geom.f90'
    'src/core/ui_vector.f90'
    'src/core/ui_svg.f90'
    'src/core/ui_font_ttf.f90'
    'src/io/ui_img.f90'
    'src/gfx/ui_gfx.f90'
    'src/gfx/ui_gfx_sw.f90'
    'src/plat/ui_plat.f90'
    'app/main.f90'
)

New-Item -ItemType Directory -Force -Path out | Out-Null

switch ($Compiler) {
    'gfortran' {
        $fc = 'gfortran'
        $flags = @('-O2', '-std=f2023', '-cpp', '-Wall', '-Wextra', '-J', 'out', '-I', 'out')
    }
    'ifx' {
        $fc = 'ifx'
        $flags = @('-O2', '-module:out', '-I', 'out')
    }
}

Write-Host "building with $fc ..." -ForegroundColor Cyan
& $fc @flags '-o' 'out/fui_demo.exe' @sources
if ($LASTEXITCODE -ne 0) { throw "$fc failed with exit code $LASTEXITCODE" }
Write-Host 'ok -> out/fui_demo.exe' -ForegroundColor Green

if ($Run) {
    Push-Location out
    try { & ./fui_demo.exe } finally { Pop-Location }
}

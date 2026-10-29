# Build the fui windowed demo (SDL2 + OpenGL 3.3 core).
#
#   .\build_window.ps1
#   .\build_window.ps1 -Run
#
# Requires the Vulkan SDK (SDL2 import lib + DLL); override with -Sdk.

param(
    [string] $Sdk = 'D:\VulkanSDK\1.4.341.1',
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
    'src/core/ui_font_os.f90'
    'src/io/ui_img.f90'
    'src/gfx/ui_gfx.f90'
    'src/gfx/ui_gfx_gl.f90'
    'src/plat/ui_plat.f90'
    'src/plat/ui_plat_sdl2.f90'
    'app/show.f90'
)

New-Item -ItemType Directory -Force -Path out | Out-Null

$sdlDll = Join-Path $Sdk 'Bin\SDL2.dll'
$sdlLib = Join-Path $Sdk 'Lib\SDL2.lib'
if (-not (Test-Path $sdlDll)) { throw "SDL2.dll not found under $Sdk" }

Copy-Item $sdlDll (Join-Path $PSScriptRoot 'out\SDL2.dll') -Force

$flags = @('-O2', '-std=f2023', '-cpp', '-J', 'out', '-I', 'out')

Write-Host 'building fui_show.exe ...' -ForegroundColor Cyan
New-Item -ItemType Directory -Force -Path out | Out-Null
& gcc -c -O2 -o out/fui_font.o shim/fui_font.c
if ($LASTEXITCODE -ne 0) { throw 'shim compile failed' }
& gfortran @flags '-o' 'out/fui_show.exe' @sources 'out/fui_font.o' '-lgdi32' $sdlDll
Write-Host 'ok -> out/fui_show.exe' -ForegroundColor Green

if ($Run) {
    Push-Location out
    try { & ./fui_show.exe } finally { Pop-Location }
}

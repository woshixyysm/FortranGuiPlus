# fui — status

## Working (verified on this machine)

| Area | State |
|---|---|
| Core widget system | buttons/slider/checkbox/progress/label, hot+active model, drag |
| Frameless window | own chrome, close box, always-on-top, drag by title bar |
| GPU window | SDL2 + OpenGL 3.3 core, **runs on screen**, 1 draw call/frame |
| Geometry batch | 6764 tris, matches vector renderer to 7/567600 px |
| SVG paths | M/L/H/V/C/S/Q/T/Z, abs+rel, `3.41.81` numbers — exact |
| TrueType | table dir, cmap(3,1) fmt4, loca short/long, glyf simple glyphs |
| Fill rule | **non-zero winding** (correct for TT + SVG), was even-odd |
| Static lazy pointer | `ui_global`, `app => ui_ctx_current()` |

## Open bug: some TrueType glyph outlines

Measured facts (do not re-derive):

* layout is **correct**: `"HH"` at 100px -> path x = 16.99..140.62,
  per-glyph advance 71px, `ui_ttf_text_width` = 141.99. No overlap.
* `'H'` (1 contour, starts on-curve) renders **correctly**.
* lowercase with repeated flag runs (`e`, `c`, `t`, `x`) come out with
  spurious connecting edges inside the polygon -> the outline itself is
  wrong, not the fill.
* glyph offset verified against an independent parse
  (`glyf 147776 + loca[72] 12902 = 160678`, `ncont=2`, `endpts 18/25`).
* `ilen`/flags pointer verified: `off+13+2*ncont+ilen = 155168` for 'H'.

=> next suspect is the REPEAT flag decode in `ttf_glyph_path`
(`btest(flags(i),3)` block) or the `X_SHORT`/`Y_SHORT` sign handling for
glyphs whose flag stream mixes short and long deltas.

## Not started

* **Dynamic Color / Monet**: generate a tonal palette from a seed colour.
  Source-Material uses HCT/CAM16; an OKLab approximation is enough for UI:
  seed -> hue/chroma -> 13 tonal stops (0,10,...,100) -> MD3 roles
  (primary, on-primary, primary-container, surface,
  surface-container-low.., outline, ...).
* **Mica**: layered translucent backdrop. A plain GL swapchain cannot
  sample the desktop, so the practical emulation is a tinted base + two
  low-contrast gradients + subtle noise, drawn by a `ui_backdrop` pass
  before the UI batch.
* **MD3 / MIUIX widget variants**: needs the theme layer split into
  `ui_theme` (roles + metrics) plus per-flavour modules, and rounded
  rectangles built from 1 centre rect + 4 corner circles so both the
  software and GL backends need no change.

## Build

```
.\build.ps1                 # headless demo  -> out/fui_demo.exe
.\build_window.ps1 -Run     # GPU window     -> out/fui_show.exe
make / fpm run demo
```

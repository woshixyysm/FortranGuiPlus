/* fui_font.c - minimal font shim for the fui framework.
 *
 * Why this exists: hand-decoding the TrueType 'glyf' point stream in Fortran
 * kept producing a constant offset on every outline. The OS already has a
 * correct scaler, so we ask it instead of re-deriving the format.
 *
 * GGO_NATIVE returns the outline as TTPOLYGONHEADER + TTPOLYCURVE, which is
 * exactly quadratic segments - the same thing 'glyf' stores, already parsed,
 * with composite glyphs expanded and the code page handled.
 *
 * Exported ABI (no structs, so Fortran needs no layout knowledge):
 *
 *   int   fui_font_open(const char *path, int px);
 *   void  fui_font_close(void);
 *   float fui_font_advance(unsigned cp);
 *   int   fui_font_glyph(unsigned cp, int *kinds, float *pts, int cap);
 *
 * kinds[i]: 0 = move (pts 0,1), 1 = line (pts 2,3), 2 = quad (0,1 ctrl, 2,3 end)
 * pts: 4 floats per segment. Returns segment count, or -1 on error.
 */
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct {
    unsigned char kind;
    float x0, y0, x1, y1;
} fui_seg;

static HDC g_dc = NULL;
static HFONT g_font = NULL;
static int g_px = 32;

#define FUI_MAX_SEG 4096
static fui_seg g_seg[FUI_MAX_SEG];
static int g_nseg = 0;

static void push_seg(int kind, float x0, float y0, float x1, float y1)
{
    if (g_nseg >= FUI_MAX_SEG) return;
    g_seg[g_nseg].kind = (unsigned char)kind;
    g_seg[g_nseg].x0 = x0;
    g_seg[g_nseg].y0 = y0;
    g_seg[g_nseg].x1 = x1;
    g_seg[g_nseg].y1 = y1;
    g_nseg++;
}

/* --- minimal sfnt name-table read to get the family name ------------- */
static int read_family(const unsigned char *d, int len, char *out, int cap)
{
    int ntab, i;
    if (len < 12) return 0;
    ntab = d[4] * 256 + d[5];
    for (i = 0; i < ntab; i++) {
        int rec = 12 + i * 16;
        int off, count, stroff, j;
        if (rec + 16 > len) break;
        if (memcmp(d + rec, "name", 4) != 0) continue;
        off = (d[rec+8] << 24) | (d[rec+9] << 16) | (d[rec+10] << 8) | d[rec+11];
        if (off + 6 > len) return 0;
        count  = d[off+2] * 256 + d[off+3];
        stroff = off + (d[off+4] * 256 + d[off+5]);
        for (j = 0; j < count; j++) {
            int r = off + 6 + j * 12;
            int plat, enc, name, sl, so, k, n = 0;
            if (r + 12 > len) break;
            plat = d[r] * 256 + d[r+1];
            enc  = d[r+2] * 256 + d[r+3];
            name = d[r+6] * 256 + d[r+7];
            sl   = d[r+8] * 256 + d[r+9];
            so   = d[r+10] * 256 + d[r+11];
            if (name != 1) continue;
            if (plat != 3 || enc != 1) continue;
            for (k = 0; k + 1 < sl && n < cap - 1; k += 2) {
                int c = stroff + so + k;
                if (c + 1 >= len) break;
                out[n++] = (char)d[c+1];    /* UTF-16BE -> ascii */
            }
            out[n] = 0;
            return 1;
        }
    }
    return 0;
}

int fui_font_open(const char *path, int px)
{
    FILE *f;
    unsigned char *buf;
    long len;
    char family[128];

    if (g_font) { DeleteObject(g_font); g_font = NULL; }

    f = fopen(path, "rb");
    if (!f) return -1;
    fseek(f, 0, SEEK_END);
    len = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (len <= 0 || len > 64 * 1024 * 1024) { fclose(f); return -2; }
    buf = (unsigned char *)malloc((size_t)len);
    if (!buf) { fclose(f); return -3; }
    if (fread(buf, 1, (size_t)len, f) != (size_t)len) { free(buf); fclose(f); return -4; }
    fclose(f);

    family[0] = 0;
    if (!read_family(buf, (int)len, family, (int)sizeof(family))) {
        fprintf(stderr, "[fui_font] name table parse failed\n");
        free(buf);
        return -5;
    }
    fprintf(stderr, "[fui_font] family='%s'\n", family);
    free(buf);

    AddFontResourceExA(path, FR_PRIVATE, 0);

    if (!g_dc) g_dc = CreateCompatibleDC(NULL);
    if (!g_dc) { fprintf(stderr, "[fui_font] CreateCompatibleDC failed\n"); return -6; }

    g_px = px;
    g_font = CreateFontA(-px, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE,
                         DEFAULT_CHARSET, OUT_TT_PRECIS, CLIP_DEFAULT_PRECIS,
                         ANTIALIASED_QUALITY, DEFAULT_PITCH | FF_DONTCARE, family);
    if (!g_font) { fprintf(stderr, "[fui_font] CreateFontA failed\n"); return -7; }

    SelectObject(g_dc, g_font);
    {
        GLYPHMETRICS gm;
        MAT2 m = { {0,1},{0,0},{0,0},{0,1} };
        if (GetGlyphOutlineW(g_dc, 'H', GGO_METRICS, &gm, 0, NULL, &m) == GDI_ERROR)
            fprintf(stderr, "[fui_font] GetGlyphOutline failed for 'H'\n");
        else
            fprintf(stderr, "[fui_font] ok, advance('H')=%ld\n", (long)gm.gmCellIncX);
    }
    return 0;
}

void fui_font_close(void)
{
    if (g_font) { DeleteObject(g_font); g_font = NULL; }
    if (g_dc) { DeleteDC(g_dc); g_dc = NULL; }
}

float fui_font_advance(unsigned cp)
{
    GLYPHMETRICS gm;
    MAT2 m = { {0,1},{0,0},{0,0},{0,1} };
    DWORD r;
    if (!g_dc || !g_font) return 0.0f;
    r = GetGlyphOutlineW(g_dc, (UINT)cp, GGO_METRICS, &gm, 0, NULL, &m);
    if (r == GDI_ERROR) return 0.0f;
    return (float)gm.gmCellIncX;
}

static float fx(const POINTFX *p) { return (float)(p->x.value + p->x.fract / 65536.0); }
static float fy(const POINTFX *p) { return (float)(p->y.value + p->y.fract / 65536.0); }

int fui_font_glyph(unsigned cp, int *kinds, float *pts, int cap)
{
    MAT2 m = { {0,1},{0,0},{0,0},{0,1} };
    GLYPHMETRICS gm;
    DWORD size, got;
    unsigned char *buf, *p, *end;

    if (!g_dc || !g_font) return -1;

    memset(&gm, 0, sizeof(gm));
    size = GetGlyphOutlineW(g_dc, (UINT)cp, GGO_NATIVE, &gm, 0, NULL, &m);
    if (size == GDI_ERROR) {
        fprintf(stderr, "[fui_font] glyph %u GGO_NATIVE size failed\n", cp);
        return -1;
    }
    if (size == 0) return 0;                       /* blank glyph, e.g. space */

    buf = (unsigned char *)malloc(size);
    if (!buf) return -2;
    memset(&gm, 0, sizeof(gm));
    got = GetGlyphOutlineW(g_dc, (UINT)cp, GGO_NATIVE, &gm, size, buf, &m);
    if (got == GDI_ERROR) {
        fprintf(stderr, "[fui_font] glyph %u GGO_NATIVE data failed (size %lu)\n",
                cp, (unsigned long)size);
        free(buf);
        return -3;
    }

    g_nseg = 0;
    p = buf;
    end = buf + size;

    while (p + sizeof(TTPOLYGONHEADER) <= end) {
        TTPOLYGONHEADER *ph = (TTPOLYGONHEADER *)p;
        unsigned char *cend;
        POINTFX cur;

        if (ph->cb == 0) break;
        cend = p + ph->cb;
        if (cend > end) break;

        cur = ph->pfxStart;
        push_seg(0, fx(&cur), fy(&cur), 0, 0);

        {
            unsigned char *q = p + sizeof(TTPOLYGONHEADER);
            while (q + 4 <= cend) {
                TTPOLYCURVE *cv = (TTPOLYCURVE *)q;
                int n = cv->cpfx;
                int i;
                unsigned char *next = q + 2 * sizeof(WORD) + n * sizeof(POINTFX);

                if (n <= 0) { q = next; continue; }

                if (cv->wType == TT_PRIM_LINE) {
                    for (i = 0; i < n; i++) {
                        POINTFX e = cv->apfx[i];
                        push_seg(1, 0, 0, fx(&e), fy(&e));
                        cur = e;
                    }
                } else {
                    /* quadratic spline: last point is on-curve, the rest are
                       control points, consecutive controls imply a midpoint */
                    i = 0;
                    while (i < n) {
                        POINTFX ctrl, ep;
                        if (i == n - 1) {
                            ep = cv->apfx[i];
                            push_seg(1, 0, 0, fx(&ep), fy(&ep));
                            cur = ep;
                            i++;
                        } else {
                            ctrl = cv->apfx[i];
                            if (i + 1 == n - 1) {
                                ep = cv->apfx[i + 1];
                            } else {
                                ep.x.value = (ctrl.x.value + cv->apfx[i+1].x.value) / 2;
                                ep.x.fract = 0;
                                ep.y.value = (ctrl.y.value + cv->apfx[i+1].y.value) / 2;
                                ep.y.fract = 0;
                            }
                            push_seg(2, fx(&ctrl), fy(&ctrl), fx(&ep), fy(&ep));
                            cur = ep;
                            i++;
                        }
                    }
                }
                q = next;
            }
        }
        p = cend;
    }

    {
        int n = g_nseg < cap ? g_nseg : cap;
        int i;
        for (i = 0; i < n; i++) {
            kinds[i] = g_seg[i].kind;
            pts[4*i+0] = g_seg[i].x0;
            pts[4*i+1] = g_seg[i].y0;
            pts[4*i+2] = g_seg[i].x1;
            pts[4*i+3] = g_seg[i].y1;
        }
        if (g_nseg == 0)
            fprintf(stderr, "[fui_font] glyph %u produced 0 segments (size %lu)\n",
                    cp, (unsigned long)size);
        free(buf);
        return n;
    }
}

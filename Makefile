# fui - immediate-mode GUI framework in pure Fortran
#
#   make                 build with gfortran
#   make FC=ifx          build with Intel ifx
#   make fpm             build with fpm
#   make run             build and run
#   make clean

FC      ?= gfortran
BUILD   := out
SRC     := src/core/ui_kinds.f90 src/core/ui_font.f90 src/core/ui_draw.f90 \
           src/core/ui_input.f90 src/core/ui.f90 src/core/ui_global.f90 src/core/ui_geom.f90 \
           src/core/ui_vector.f90 src/core/ui_svg.f90 src/core/ui_font_ttf.f90 src/core/ui_color_hct.f90 \
           src/io/ui_img.f90 src/gfx/ui_gfx.f90 src/gfx/ui_gfx_sw.f90 \
           src/plat/ui_plat.f90 app/main.f90
EXE     := $(BUILD)/fui_demo

ifeq ($(notdir $(FC)),ifx)
  FFLAGS ?= -O2 -module:$(BUILD) -I$(BUILD)
else
  FFLAGS ?= -O2 -std=f2023 -cpp -Wall -Wextra -J$(BUILD) -I$(BUILD)
endif

.PHONY: all run clean fpm

all: $(EXE)

$(EXE): $(SRC)
	@mkdir -p $(BUILD)
	$(FC) $(FFLAGS) -o $(EXE) $(SRC)

run: $(EXE)
	cd $(BUILD) && ./fui_demo

fpm:
	fpm run demo

clean:
	rm -rf $(BUILD) build *.mod frame_*.ppm frame_*.bmp

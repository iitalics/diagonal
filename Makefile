BUILD_DIR     = .build
DUNE_TARG_DIR = $(shell pwd)/${BUILD_DIR}/$(shell opam switch show)
DUNE_BUILD    = dune build --build-dir=${BUILD_DIR}

BROWSER ?= xdg-open

JS     = dia_js/dia.bc.js
STATIC = $(wildcard dia_js/*.html dia_js/*.css)
RES    =				\
		dia_js/res/roundor.otf	\
		dia_js/res/nunito.ttf	\
		dia_js/res/space_mono.ttf \
		dia_js/res/space_mono_bold.ttf \
		dia_js/res/sprites.png	\
		dia_js/res/map_stone.png

all:
	${DUNE_BUILD} ${JS} ${STATIC} ${RES}

open-preview:
	${BROWSER} ${DUNE_TARG_DIR}/dia_js/index.html

clean:
	rm -rf ${BUILD_DIR}

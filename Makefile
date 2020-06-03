BUILD_DIR     = .build
DUNE_TARG_DIR = $(shell pwd)/${BUILD_DIR}/$(shell opam switch show)
DUNE_BUILD    = dune build --build-dir=${BUILD_DIR}

BROWSER ?= xdg-open

JS     = dia_js/dia.bc.js
STATIC = $(wildcard dia_js/*.html dia_js/*.css)

all:
	${DUNE_BUILD} ${JS} ${STATIC}

open-preview:
	${BROWSER} ${DUNE_TARG_DIR}/dia_js/index.html

clean:
	rm -rf ${BUILD_DIR}

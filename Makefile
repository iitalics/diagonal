BUILD_DIR     = .build
DUNE_TARG_DIR = $(shell pwd)/${BUILD_DIR}/$(shell opam switch show)
DUNE_BUILD    = dune build --profile=release --build-dir=${BUILD_DIR}

BROWSER ?= xdg-open

JS   = dia_js/dia.bc.js
HTML = $(wildcard dia_js/*.html)

all:
	${DUNE_BUILD} ${JS} ${HTML}

open-preview:
	${BROWSER} ${DUNE_TARG_DIR}/dia_js/index.html

clean:
	rm -rf ${BUILD_DIR}

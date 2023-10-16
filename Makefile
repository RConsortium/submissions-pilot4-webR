SHELL = /bin/sh

SOURCE_DIR = app
BUILD_DIR = site

build:
	Rscript -e 'shinylive::export("${SOURCE_DIR}", "${BUILD_DIR}")'
	cp -r www/* ${BUILD_DIR}

run:
	Rscript -e 'httpuv::runStaticServer("${BUILD_DIR}", browse = TRUE)'

clean:
	rm -r ${BUILD_DIR} || exit 0

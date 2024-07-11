DOCS := \
	specs.adoc

DATE ?= $(shell date +%Y-%m-%d)
VERSION ?= v1.0.0
REVMARK ?= Release

BUILD_DIR := build

DOCS_PDF := $(DOCS:%.adoc=%.pdf)
DOCS_HTML := $(DOCS:%.adoc=%.html)

XTRA_ADOC_OPTS :=
ASCIIDOCTOR_PDF := asciidoctor-pdf
ASCIIDOCTOR_HTML := asciidoctor
OPTIONS := --trace \
           -a compress \
           -a mathematical-format=svg \
           -a revnumber=${VERSION} \
           -a revremark=${REVMARK} \
           -a revdate=${DATE} \
           -a pdf-fontsdir=fonts \
           -a pdf-theme=themes/pdf.yml \
           -a stylesheet=themes/asciidoctor-default.css \
           $(XTRA_ADOC_OPTS) \
		   -D build \
           --failure-level=ERROR
REQUIRES := --require=asciidoctor-bibtex \
            --require=asciidoctor-diagram \
            --require=asciidoctor-mathematical

.PHONY: all build clean build-container build-no-container build-docs

all: build

build: $(DOCS_PDF) $(DOCS_HTML)


%.pdf: %.adoc
	$(ASCIIDOCTOR_PDF) $(OPTIONS) $(REQUIRES) $<

%.html: %.adoc
	$(ASCIIDOCTOR_HTML) $(OPTIONS) $(REQUIRES) $<

clean:
	@echo "Cleaning up generated files..."
	rm -rf $(BUILD_DIR)
	@echo "Cleanup completed."


DATE ?= $(shell date +%Y-%m-%d)
VERSION ?= v1.0.0
REVMARK ?= Release

BUILD_DIR := build

DOCS_ADOC := $(wildcard *.adoc)
DOCS_PNG := $(wildcard diag/*.png)
DOCS_SRCS := $(DOCS_ADOC) $(DOCS_PNG)

DOCS_PDF := build/specs.pdf
DOCS_HTML := build/specs.html

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


$(DOCS_PDF): $(DOCS_SRCS)
	$(ASCIIDOCTOR_PDF) $(OPTIONS) $(REQUIRES) $<

$(DOCS_HTML): $(DOCS_SRCS)
	$(ASCIIDOCTOR_HTML) $(OPTIONS) $(REQUIRES) $<

publish: $(DOCS_PDF) $(DOCS_HTML)
	cp $(DOCS_HTML) index.html

clean:
	@echo "Cleaning up generated files..."
	rm -rf $(BUILD_DIR)
	@echo "Cleanup completed."

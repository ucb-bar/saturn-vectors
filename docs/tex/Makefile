# Define variables
LATEX=pdflatex -halt-on-error
BIBTEX=bibtex
SRC=specs.tex
SRCS=$(wildcard *.tex)
BIB=../refs.bib
OUT=specs.pdf

# Default target
all: $(OUT)

# Rule to generate PDF
$(OUT): $(SRC) $(BIB) $(SRCS)
	$(LATEX) $(SRC)     # Run pdflatex the first time to generate .aux file
	$(BIBTEX) $(basename $(SRC))  # Run bibtex to generate the bibliography
	$(LATEX) $(SRC)     # Run pdflatex the second time to include citations
	$(LATEX) $(SRC)     # Run pdflatex a third time to ensure proper references

# Clean up auxiliary files
clean:
	rm -f *.aux *.bbl *.blg *.log *.out *.toc *.pdf

# Force a rebuild
rebuild: clean all

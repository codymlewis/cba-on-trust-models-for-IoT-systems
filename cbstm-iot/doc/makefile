all: clean doc display

clean:
	$(RM) CBSTM-IoT.aux CBSTM-IoT.log CBSTM-IoT.out CBSTM-IoT.pdf

doc:
	pdflatex CBSTM-IoT.tex; \
	bibtex CBSTM-IoT; \
	pdflatex CBSTM-IoT.tex; \
	pdflatex CBSTM-IoT.tex; \

display:
	zathura CBSTM-IoT.pdf

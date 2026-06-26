export SOURCE_DATE_EPOCH := `date +%s`

[working-directory: 'writings']
latex:
    pdflatex -output-directory build/ thesis.tex
    mv build/thesis.pdf thesis.pdf

[working-directory: 'writings']
bib:
    biber -output-directory build/ build/thesis.bcf
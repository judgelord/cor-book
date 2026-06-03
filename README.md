This repo contains all of the files needed to render the book, including replication files for each chapter that create sub-folders of figures, tables, and model objects for that chapter.

## Folders 

- `assets/` is a folder for helpful files, including: 
  - `cor.bib` (the bib file)
  - `cover.png` (the book cover)
- `code/` is a folder for code 
- `data/` is a folder for data 
- `docs/ `is the folder where the output files are generated (including pdf and html) it gets re-written when we render/build the book, so don't put things in here
- `_extensions/` is a folder for pdf formatting 
- `figs/` is a folder for figures. The replication files put figures in here, and the chapters pull figures from here.
- `models/`1 is a folder for saved model objects. The replication files put models in here, and the chapters pull models from here.
- `tables/` is a folder for saved tables. The replication files put tables in here, and the chapters pull tables from here.
- `slides/` is a folder for slide decks

## Files

- `_quarto.yml` specifies which files to compile into the book in which order
- `index.md` is the home page for the book website
- `make_book.R` is code to render and preview the book 
- `article-version.qmd` is the article version for APSA
\+ files for each chapter and the appendix



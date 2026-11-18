# make-causal-figs.R
# Renders causal.qmd, then copies the mermaid figures to figs/ with descriptive names.
#
# Key (mermaid output numbers are NOT in chunk order; check figs/ after each render):
#   mermaid-figure-1.png -> causal-overall.png      Overall framework flowchart
#   mermaid-figure-2.png -> causal-old-overall.png  Old combined figure (not used)
#   mermaid-figure-3.png -> causal-strategy.png     Strategy flowchart
#   mermaid-figure-4.png -> causal-capacity.png     Capacity flowchart
#   mermaid-figure-5.png -> causal-demand.png       Demand flowchart
#
# demand-pyramid.png / .pdf is written directly to figs/ by an R chunk in causal.qmd.

quarto::quarto_render("causal.qmd")

fig_names <- c(
  "mermaid-figure-6.png" = "causal-demand.png",
  "mermaid-figure-2.png" = "causal-demand-casework.png",
  "mermaid-figure-3.png" = "causal-old-overall.png",
  "mermaid-figure-4.png" = "causal-strategy.png",
  "mermaid-figure-5.png" = "causal-capacity.png",
  "mermaid-figure-1.png" = "causal-overall.png"

)
src <- file.path("causal_files", "figure-html", names(fig_names))
stopifnot(all(file.exists(src)))
file.copy(src, file.path("figs", fig_names), overwrite = TRUE)


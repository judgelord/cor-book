# if you want, preview the book in HTML (the pdf is in the /docs/ folder)
quarto::quarto_preview()

# to stop the preview (required to run other code)
quarto::quarto_preview_stop()

# send to git (just Devin for now)
if(F){
  system(command = "git add docs")
  system(command = "git restore --staged docs/interbranch.pdf")
  system(command = "git restore --staged docs/slides/interbranch.html")
  system(command = 'git commit -m "Publish site to docs/"')
  system(command = "git push")
}

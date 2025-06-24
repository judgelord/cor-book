# you need to provide and email to authorize google drive
email = "devin.jl@gmail.com"

library(googledrive)
drive_auth(email = email)

# get chapters from google drive and render the book
source("code/get_chapters.R")

# if you want, preview the book in HTML (the pdf is in the /docs/ folder)
quarto::quarto_preview()

# to stop the preview (required to run other code)
quarto::quarto_preview_stop()

# send to git (just Devin for now)
if(F){
  system(command = "git add docs")
  system(command = 'git commit -m "Publish site to docs/"')
  system(command = "git push")
}

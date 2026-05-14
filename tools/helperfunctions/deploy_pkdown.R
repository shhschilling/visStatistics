# 1. Build the site locally using your updated pkgdown config
message("Building site...")
pkgdown::build_site(install = FALSE)

# 2. Add the docs folder to git (even if ignored by .gitignore)
message("Staging docs folder...")
system("git add docs -f")

# 3. Commit the changes
message("Committing changes...")
system('git commit -m "Manual site update via deploy script"')

# 4. The 'Subtree' Force Push
# This takes only the 'docs' folder and shoves it into the 'gh-pages' branch
message("Pushing to GitHub...")
system("git push origin `git subtree split --prefix docs master`:gh-pages --force")

message("Done! Give it 30 seconds and refresh your browser.")

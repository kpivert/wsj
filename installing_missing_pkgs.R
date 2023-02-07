# Installing Packges Not installed with New RSTudio ----

# 001 Used ls in CLI to Find Missing Packages ----

pkgs <- 
  read.csv("../../pkgs_to_install.txt")

pkgs <- 
  pkgs$AER |> 
  c("AER")

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])

missing <- setdiff(pkgs, installedpkgs)
install.packages(missing)
update.packages()
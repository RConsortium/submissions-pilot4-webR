# Structure
- app: webr ready version
- shiny: hard copy of the rhino version
- site: compiled webr version

# Compiling
Use the `app` folder version for compilation. This version should run normally as a traditional shiny application with minimal changes.
1. include the www folder in the root of the project
2. Switch the data loading process in `app.R` (see line ~60 in the app.R file)

1. install needed dependencies to compile and serve
```
install.packages("pak")
pak::pak("posit-dev/r-shinylive")
pak::pak("rstudio/httpuv")
```

2. Export the app version into webR
```
shinylive::export("app", "site")
```

3. serve locally to test
```
httpuv::runStaticServer("site")
```

A deployed version can be found here: https://brilliant-elf-2da930.netlify.app/
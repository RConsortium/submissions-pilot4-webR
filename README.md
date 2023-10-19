# Structure
- app: webr ready version
- site: compiled webr version

# Compiling
Use the `app` folder version for compilation. This version should run normally as a traditional shiny application with minimal changes.

1. install needed dependencies to compile and serve
```
install.packages("pak")
pak::pak("posit-dev/r-shinylive")
pak::pak("rstudio/httpuv")
```

2. Export the app version into webR by running the `build.R` script in the root folder

3. serve locally to test
```
httpuv::runStaticServer("site")
```

A deployed version can be found here: https://brilliant-elf-2da930.netlify.app/
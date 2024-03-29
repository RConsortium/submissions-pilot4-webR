# Structure
- app: Shiny application.
- site: compiled webr version.
- build.R: Helper script to generate the site output folder from the app folder.

---

# Development Setup

This repository was last updated using R version 4.2.3. Dependencies are managed by [`{renv}`](https://rstudio.github.io/renv/articles/renv.html) version `1.0.3`. Once you have those installed on your system, you can restore the R package library by running `renv::restore()` in the R console.

# Compiling a webR version
Use the applicaion in the `app` folder version for compilation. This version can also be run normally as a traditional shiny application.

1. install needed dependencies to compile and serve
```
install.packages("pak")
pak::pak("posit-dev/r-shinylive")
pak::pak("rstudio/httpuv")
```

2. Export the app version into webR by running the `build.R` script in the root folder. If you would like to try to serve all package dependencies locally instead of using the CRAN repository at runtime, see the section `Generating a local version of the package dependencies (Experimental)` at the end of this file.

3. serve locally to test
```
httpuv::runStaticServer("site")
```

Other server options should also work, for example using python from the command line:
```
# If Python version returned above is 3.X
# On Windows, try "python -m http.server" or "py -3 -m http.server"

python3 -m http.server
# If Python version returned above is 2.X
python -m SimpleHTTPServer
```

A deployed version can be found here: https://brilliant-elf-2da930.netlify.app/

---

# webR compatibility limitations, issues and restrictions

Currently there are still some limitations when it comes to webR and shiny live.

### Incompatible packages

Some packages used in the previous application versions are not available on CRAN. In adition to that, not all CRAN packages are compatible with webR itself. In the case of this project, that includes:

- teal
- teal.data
- cowplot
- shinyWidgets

To circumvent this, non working dependencies and their functionality was either removed, replaced or shimmed. This includes:

- teal - Removing teal required redoing some of the UI manually, simplifying the module loading system for each view, as well as simplifying the filtering in the KM Plot tab.

- teal.data - A shim was created for the dataset R6 class. This includes the minimum functionality to work for this application. The adam data loading was also tweaked to allow loading files in a browser environment.

- cowplot - While compatible with webR, a deployed version was not supporting the package because of some low level dependency calls. Since its use was small and limited to the KM Plot tab, some of its broken functionality was replicated in HTML.

- shinyWidgets - Not compatible with webR. Widgets used particularly for filtering the KM Plot were simplified and replaced with base widgets and additional HTML.


### Other notes
- This repository was created using R version 4.3

- Reading files had to be adjusted to use a `download.file` system instead. This is because in the browser a working directory does not tecnically exist.

- Update functions for widgets are not working and cannot be used in the webR version. For most cases this was replaced with the slower (but working) re-rendering of the widgets in question (`renderUI`, `uiOutput`)

- Building the webR version using `{shinylive}` requires the folder to contain ONLY R script files. To manually use the export functionality of `{shinylive}` this means that any static files, javascript, stylesheets or configuration files cannot be in the project folder.

- The provided `build.R` script will strip these files from the working directory for compilation and copy them after to the export folder to be available in the webR version.

---

### Generating a local version of the package dependencies (Experimental)
Currently there is no build in mechanism to serve packages locally, but we can tweak the exported version to allow this. After running `build.R` to generate the app, you can run `build_offline_repo.R` to download all the package files and replace the repo location. run this script after each successful build to update the output bundle.

NOTE: Currently a list of package files is stored in that file. These are collected manually by inspecting the browser network tab at run time. When adding new dependencies to the application those will need to be manually added on the script.

NOTE: After downloading the folder at least once it is advised to do a backup of its contents (100mb)

WARNING: This is a experimental work around. WebR will most likely support something similar by default.

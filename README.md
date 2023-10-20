# Structure
- app: Shiny application.
- site: compiled webr version.
- build.R: Helper script to generate the site output folder from the app folder.

---

# Compiling a webR version
Use the applicaion in the `app` folder version for compilation. This version can also be run normally as a traditional shiny application.

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

- Reading files had to be adjusted to use a `download.file` system instead. This is because in the browser a working directory does not tecnically exist.

- Update functions for widgets are not working and cannot be used in the webR version. For most cases this was replaced with the slower (but working) re-rendering of the widgets in question (`renderUI`, `uiOutput`)

- Building the webR version using `{shinylive}` requires the folder to contain ONLY R script files. To manually use the export functionality of `{shinylive}` this means that any static files, javascript, stylesheets or configuration files cannot be in the project folder.

- The provided `build.R` script will strip these files from the working directory for compilation and copy them after to the export folder to be available in the webR version.
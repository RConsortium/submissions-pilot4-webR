## Overview

Building upon the Submissions [Pilot 2](https://github.com/RConsortium/submissions-pilot2) in which a Shiny application created as an R package was successfully transferred to the FDA using the eCTD gateway protocol, the objective of Pilot 4 is to explore novel technologies to bundle the application along with the necessary execution dependencies and streamline the transfer and execution procedures. The specific technologies targeted in this pilot are the following:

* __Containers__: A mechanism to encapsulate a piece of software alongside the environment used for its dependencies and execution. The end user simply needs a container runtime installed on their system to execute a container.
* __WebAssembly__: A framework inspired by assembly in which applications developed in languages such as JavaScript, Python, and now R can be compiled into a self-contained native bundle that can be executed directly in a user's modern web browser, without requiring additional software on their host system.

This repository contains the WebAssembly version of the application.

## Important Links

* Preview version of the application: <https://submissions-pilot4-webr.netlify.app/>
* Repository with ECTD materials: <https://github.com/RConsortium/submissions-pilot4-webR-to-fda>
* Preview version of the Analysis Data Reviewer Guide (ADRG): <https://rpodcast.quarto.pub/pilot4-webassembly-adrg/>

## Development Setup

### Package Environment

This application uses R version 4.2.3. Dependencies are managed by [`{renv}`](https://rstudio.github.io/renv/articles/renv.html) version `1.0.3`. Once you have those installed on your system, you can restore the R package library by running `renv::restore()` in the R console.

### Building and running the application

All supporting functions for building and running the application are contained in the `utils.R` script. Ensure that you load that in your R session with `source("utils.R")` before you proceed.

```r
# build application
build_app()
```

This will compile the application into a new sub-directory `_site` by default. Note that this directory is not tracked in version control.

```r
# run webassembly application
run_app_webassembly()
```

This will execute a server-side process in your R session and the application will be viewable on your local system. Open your local web browser and browse to this address: `localhost:7654`.

If you wish to run the application in the traditional method (with Shiny), you can run the following snippet:

```r
# run traditional Shiny application
run_app_shiny()
```

### Assembling ECTD bundle

To build a custom `.zip` archive of the application for eventual transfer in the ECTD gateway, run the following snippet:

```r
# create application bundle zip archive
create_ectd_bundle()
```

By default a new file called `r4app.zip` will be created in the `ectd_bundle` sub-directory. Note that this bundle is not tracked in version control of this repository. Instead, a GitHub Action will automatically send this bundle to the [Pilot 4 ECTD GitHub repository](https://github.com/RConsortium/submissions-pilot4-webR-to-fda) when the `main` branch of this repository is updated.

### Alternative server process

While this repository positions R as the method for launching a local web server to run the application, other server options should also work. For example using Python from the command line:

```
# Navigate to the directory of the site deployment (_site by default)
# If Python version returned above is 3.X
# On Windows, try "python -m http.server" or "py -3 -m http.server"

python3 -m http.server 7654
# If Python version returned above is 2.X
python -m SimpleHTTPServer 7654
```

Open your local web browser and browse to this address: `localhost:7654`.

### Automation

A set of GitHub actions have been created to automate the following processes:

* [`publish-adrg.yaml`](https://github.com/RConsortium/submissions-pilot4-webR/blob/main/.github/workflows/publish-adrg.yaml): Publish a preview version of the ADRG to [Quarto Pub](https://quartopub.com/) at <https://rpodcast.quarto.pub/pilot4-webassembly-adrg/>.
* [`publish-app.yaml`](https://github.com/RConsortium/submissions-pilot4-webR/blob/main/.github/workflows/publish-app.yaml): Publish a preview version of web-assembly application to [Netlify](https://netlify.com) at <https://submissions-pilot4-webr.netlify.app/>.
* [`publish-ectd-bundle.yaml`](https://github.com/RConsortium/submissions-pilot4-webR/blob/main/.github/workflows/publish-ectd-bundle.yaml): Compile the application source files and associated helper functions to a `.zip` archive and automatically commit the bundle to the `main` branch of the [Pilot 4 ECTD GitHub repository](https://github.com/RConsortium/submissions-pilot4-webR-to-fda).

---

## webR compatibility limitations, issues and restrictions

Currently there are still some limitations when it comes to webR and `{shinylive}`.

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

- This repository was orignally created using R version 4.3

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

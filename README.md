# Pilot 2 Submission based on Rhino framework

The R Consortium R submission [Pilot 2 Project](https://github.com/RConsortium/submissions-pilot2) is to test the concept that a Shiny application created with the R-language can be successgully bundled into a submission package and transferred successfully to FDA reviewers.

This repo tries to reproduce the same Shiny application using the Rhino framework.

# Deployment

This application is deployed at [pilot-2-submission-rhino](https://connect.appsilon.com/rhino-fda-pilot/) using Git-Backed deployment on the main branch.

# App structure

```
.
├── app
│   ├── view
│   │   └── demographic_table.R
|   |   └── km_plot.R
|   |   └── primary_table.R
|   |   └── efficacy_table.R
|   |   └── completion_table.R
│   ├── logic
│   │   └── adam_data.R
│   │   └── eff_modles.R
│   │   └── formatters.R
│   │   └── helpers.R
│   │   └── kmplot_helpers.R
│   │   └── Tplyr_helpers.R
│   ├── data
│   │   └── adam
│   │       └── adadas.xpt
│   │       └── adlbc.xpt
│   │       └── adsl.xpt
│   │       └── adtte.xpt
│   ├── docs
│   │   └── about.md
│   ├── js
│   │   └── index.js
│   ├── static
│   │   └── favicon.ico
│   ├── styles
│   │   └── main.scss
│   └── app.R
├── tests
│   ├── cypress
│   │   └── integration
│   │       └── app.spec.js
│   ├── testthat
│   │
│   └── cypress.json
├── app.R
├── rhino_submission.Rproj
├── dependencies.R
├── renv.lock
├── rhino.yml
└── README.md
```

- It's a standard rhino structure. The only difference is that it's using the `legacy_entrypoint: "app_dir"` so there is a `app.R` inside the `app/` directory.

- The app uses 4 ADaM datasets as XPT files to function which are present inside the `app/data` directory. To learn more about ADaM data formats visit [here](https://docs.google.com/spreadsheets/d/1ZHm5qrrBE_fytdGW8ei1JdUSyKKithNB-e2aPP9XgeI/edit?usp=sharing)

- The different teal modules are place in their individual file inside the `app/view` directory.

- There are a few logic and calculation functions which are placed in the `app/logic` directory.

- No tests are added as of now.

- No custom CSS/JS is added as of now.

# Packing the app for the eCTD gateway submission

The submission of proprietary packages to FDA via the eCTD gateway only supports plain text files. We use the  pkglite R package to pack and unpack this Shiny/Rhino application, so that the FDA reviewers can accept and setup the submission on their systems.

The `pkglite` package was built to pack R packages to `.txt` file, therefore we have to create a simple DESCRIPTION file with just a package name to use `pkglite` to pack into a plain text file.

## Packing Shiny app to `.txt` file

Now, we can use the following code to pack our shiny app into a `.txt` file: Create a simple DESCRIPTION file, pack the app, and remove the DESCRIPTION file.
```
app_name <- "rhinosubmission"
renv_spec <- pkglite::file_spec(
  "renv",
  pattern = "^settings\\.dcf$|^activate\\.R$",
  format = "text", recursive = FALSE
)
tests_spec <- pkglite::file_tests()
app_spec <- pkglite::file_auto("app")
root_spec <- pkglite::file_spec(
  path = ".",
  pattern = "^\\.Rprofile$|^rhino\\.yml$|^renv\\.lock$|^dependencies\\.R$|^config\\.yml$|^app\\.R$|^README\\.md$|\\.Rproj$",
  all_files = TRUE,
  recursive = FALSE
)
write(paste0("Package: ", app_name), "DESCRIPTION", append=TRUE)
pkglite::collate(
  getwd(),
  renv_spec,
  tests_spec,
  app_spec,
  root_spec
) |> pkglite::pack()
file.remove("DESCRIPTION")
```

## Unpacking Shiny app from the `.txt` file

The packed `.txt` file can be unpacked into the shiny app with the help of `pkglite`
```
pkglite::unpack("rhinosubmission.txt")
```

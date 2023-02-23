# Pilot 2 Submission based on Rhino framework

The R Consortium R submission Pilot 2 Project is to test the concept that a Shiny application created with the R-language can be successgully bundled into a submission package and transferred successfully to FDA reviewers.

This repo tries to reproduce the same Shiny application using the Rhino framework.

# Deployment

This application is deployed at [pilot-2-submission-rhino](https://connect.appsilon.com/pilot-2-submission-rhino/) using Git-Backed deployment on the main branch.

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

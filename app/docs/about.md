# Introduction

This application is intended for a pilot submission to the FDA composing of a Shiny application, as part of the [R Submissions Working Group](https://rconsortium.github.io/submissions-wg/) Pilot 2. The data sets and results displayed in the application originate from the [Pilot 1 project](https://rconsortium.github.io/submissions-wg/pilot-overall.html#pilot-1---common-analyses). Below is a brief description of the Visit the **Usage Guide** for information on using the application. Below is a brief description of the application components:

#### Rhino Application

The pilot Shiny application was adapted to use the Rhino framework bringing entreprise features to this application.

The source code is available at the [Appsion/rhino-fda-pilot-2-submission](https://github.com/Appsilon/rhino-fda-pilot-2-submission/) Github repository. It hosts the source code and an overview of the documentation and procedure used to adapt the original pilot application.

By using the Rhino framework we have added to this pilot:

1. **Clear code structure**: scalable app architecture, modularization based on Box and full support for {Teal}.
2. **Code quality**: unit test for custom logic, [linting](https://en.wikipedia.org/wiki/Lint_%28software%29).
3. **Interface testing**: end-to-end tests using Cypress2 framework.
4. **Automation**: continuous integration and automated test using Github Actions, package and version management with {Renv}.
5. **Customization**: adds custom styles on top of the {Teal} framework.

### Demographic Table

In this interface, summary statistics associated with baseline clinical characteristics and other demographic factors is shown.

### KM-Plot for TTDE

A Kaplan-Meier (KM) plot of the Time to First Dermatologic Event (TTDE) with strata defined by treatment group is displayed along with an informative risk set table across time.

### Primary Table

A summary table of the primary efficacy analysis is shown for each of the time points of assessment (baseline and week 24) comparing each treatment group. The primary efficacy variable (change from baseline in ADAS Cog (11)) was analyzed using an Analysis of Covariance (ANCOVA) model with treatment and baseline value as covariates, comparing Placebo to Xanomeline High Dose.

### Efficacy Table

A summary table of an additional efficacy analysis is shown for baseline and week 20. The efficacy variable (Glucose) was analzying using ANCOVA model with treatment and baseline value as covariates, comparing Placebo to Xanomeline High Dose.

### Visit Completion

A summary table of the number of patients remaining in the treatment period for each scheduled visit from baseline to week 24.

### 

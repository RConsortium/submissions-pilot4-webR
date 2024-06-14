# Introduction

This application is intended for a pilot submission to the FDA composing of a Shiny application, as part of the
<a href="https://rconsortium.github.io/submissions-wg/" target="_blank">R Submissions Working Group</a>
Pilot&nbsp;2.
The data sets and results displayed in the application originate from the
<a href="https://rconsortium.github.io/submissions-wg/pilot-overall.html#pilot-1---common-analyses" target="_blank">Pilot&nbsp;1 project</a>.

Visit the **Usage Guide** for information on using the application.
Below is a brief description of the reasons behind the conversion to a Rhino application and the application components.

---

### Source: R Consortium

This R/Shiny application was initially developed using the
<a href="https://thinkr-open.github.io/golem/" target="_blank">Golem</a>
framework by the R Consortium submission working group.

A team at
<a href="https://appsilon.com" target="_blank">Appsilon</a>
adapted the pilot Shiny application to use the <a href="https://appsilon.github.io/rhino/" target="_blank">Rhino</a> framework bringing enterprise features to this application.

Among the participants are Ning Leng _(Roche)_, Heng Wang _(Roche)_, Mike Stakehouse _(Atorus)_, Eli Miller _(Atorus)_, Yilong Zhang _(Merck)_, Gregery Chen _(Merck)_, Eric Nantz _(Eli Lilly)_, and Sam Parmar _(Pfizer)_. Paul Schuette and Hye Soo Cho from FDA are also participating in this working group.

The full list is publicly available at the
<a href="https://rconsortium.github.io/submissions-wg/pilot2.html" target="_blank">Pilot&nbsp;2 page</a>.

The source code for the original Pilot application is publicly available at the
<a href="https://github.com/RConsortium/submissions-pilot2" target="_blank">RConsortium/submissions-pilot2</a>
Github repository.

---

### WebR Application
This current iteration focuses on using the code from past Applications to generate a standalone version based on <a href="https://docs.r-wasm.org/webr/latest/" target="_blank">webR</a> and <a href="https://posit-dev.github.io/r-shinylive/" target="_blank">shinylive</a> as a way to create a more encapsulated and server independent version of the application.

The source code is available at the
<a href="https://github.com/RConsortium/submissions-pilot4-webR" target="_blank">RConsortium/submissions-pilot4-webR</a>
Github repository. It hosts the source code and an overview of the documentation and procedure used to adapt the original pilot application.

---

## Application Views

The following views are available in the Application:

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

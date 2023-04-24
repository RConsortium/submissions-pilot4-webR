describe('app', () => {
  beforeEach(() => {
    cy.visit('/')

    // Find the tab and click to navigate
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .contains("KM plot for TTDE")
      .click()
      .invoke("attr", "href")
      .as("href_tab")

    // Make sure the correct tab is selected
    cy.get('@href_tab').then(href_tab => {

      // Look for "Data is loaded"" element
      cy.contains("Data loaded - App fully started up")

      // Define an alias that references the current active tab
      cy
        .get(`${href_tab}.tab-pane.active`)
        .should("be.visible")
        .as("active_tab")

      // Define an alias that references the active filters
      cy
        .get("#app-teal_wrapper-teal-main_ui-filter_panel-filters_overview")
        .as("filter_summary")

      // Define an alias that references the active variables
      cy
        .get("#app-teal_wrapper-teal-main_ui-filter_panel-filter_active_vars")
        .as("filter_variables")

      // Define an alias that references the section that adds new variables to
      //  the filters
      cy
        .get("#app-teal_wrapper-teal-main_ui-filter_panel-filter_add_vars")
        .should("be.visible")
        .as("add_filter")
    })

  })

  //  Filter summary
  // ######################################

  it("Filter Summary has non-zero Observations", () => {
     cy
      .get("@filter_summary")
      .within(() => {
        cy
          .get('.shiny-bound-output')
          .each(($el, index, $list) => {
            cy
              .wrap($el)
              .children()
              .should("have.length.gte", 1)
          })
      })
  })

  //  Filter variables
  // ######################################

  it("Filter variables has shiny content rendered", () => {
     cy
      .get("@filter_variables")
      .within(() => {
        cy
          .get('#app-teal_wrapper-teal-main_ui-filter_panel-filter_active_vars_contents')
          .should("have.length.gte", 1)

        cy
          .get("#app-teal_wrapper-teal-main_ui-filter_panel-ADSL_filter-filter-cards")
          .children()
          .should("have.length", 0)

        cy
          .get("#app-teal_wrapper-teal-main_ui-filter_panel-ADAS_filter-filter-cards")
          .children()
          .should("have.length", 0)

        cy
          .get("#app-teal_wrapper-teal-main_ui-filter_panel-ADTTE_filter-filter-cards")
          .children()
          .should("have.length", 0)

        cy
          .get("#app-teal_wrapper-teal-main_ui-filter_panel-ADLB_filter-filter-cards")
          .children()
          .should("have.length", 0)
      })
  })

  //  Add variables to filter
  // ######################################

  it("Add filter variables has shiny content rendered", {
    defaultCommandTimeout: 10000
  }, () => {
     cy
      .get("@add_filter")
      .within(() => {
          cy
            .get('.shiny-input-container')
            .each(($el, index, $list) => {
              cy
                .wrap($el)
                .children()
                .should("have.length.gte", 1)
            })
        })
  })

    //  Add variables to filter
  // ######################################

  it("Add filter", () => {
     cy
      .get("@add_filter")
      .within(() => {
          cy
            .get(".filter-option")
            .contains("Select variable to filter")
            .should("be.visible")
            .click()

          cy
            .get(".dropdown-menu.open li")
            .contains("Age")
            .click()
        })

    let subjects = ""

    cy
      .get("#app-teal_wrapper-teal-main_ui-filter_panel-teal_filters_info-table table tbody tr:first td:last")
      .contains(/[0-9]+\/[0-9]+/)
      .then(($el, index, $list) => {
        cy.log(`Content is ${$el[0].innerText}`)
        subjects = $el[0].innerText
      })

    // Monitor how many times summary is recalculated vias shiny:recalculated
    // event
    cy
      .get("@filter_summary")
      .within(() => {
        cy
          .get(".shiny-bound-output")
          .invoke("on", "shiny:recalculated", cy.stub().as("summary_change"))
      })

    cy
      .get("@filter_variables")
      .within(() => {

        const dataTransfer = new DataTransfer()

        cy
          .get(".filterPlotOverlayRange .shiny-plot-output")
          .children()
          .should("have.length.gte", 1)

        cy
          .get(".irs-handle.from")
          .trigger("mousedown", { button: 0, which: 1, pageX: 600, pageY: 100 })
          .trigger("mousemove", { clientX: 0, clientY: 400 })
          .trigger("mouseup")

        cy.get('@summary_change', { timeout: 20000 }).should('have.callCount', 1)

        cy
          .get(".irs-handle.from")
          .trigger("mousedown", { button: 0, which: 1, pageX: 600, pageY: 100 })
          .trigger("mousemove", { clientX: 0, clientY: 400 })
          .trigger("mouseup")

        cy.get('@summary_change', { timeout: 20000 }).should('have.callCount', 2)

        cy
          .get(".irs-handle.from")
          .trigger("mousedown", { button: 0, which: 1, pageX: 600, pageY: 100 })
          .trigger("mousemove", { clientX: 0, clientY: 400 })
          .trigger("mouseup")

        cy.get('@summary_change', { timeout: 20000 }).should('have.callCount', 3)
      })


      cy.log(`Subjects: ${subjects}`)
      cy
        .get("@filter_summary")
        .within(() => {

        // First row
        cy
          .get(
            "#app-teal_wrapper-teal-main_ui-filter_panel-teal_filters_info-table table tbody tr:first td:last",
            { timeout: 10000 }
          )
          // There
          .should("satisfy", ($el) => {
            console.log($el[0])
            console.log($el[0].innerText)
            const result = /([0-9]+)\/([0-9]+)/.exec($el[0].innerText)
            return result.length == 3 && result[1] != result[2]
          })
          .then(($el, index, $list) => {
            cy.log(`Content is ${$el[0].innerText}`)
            subjects = $el[0].innerText
          })
        })


  })


  //  Active tab
  // ######################################

  it("Active tab has shiny content rendered", () => {
    cy
      .get("@active_tab")
      .within(() => {
        cy
          .get('.shiny-bound-output')
          .each(($el, index, $list) => {
            cy
              .wrap($el)
              .children()
              .should("have.length.gte", 1)
          })
      })
  })

})

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

  it("Add filter variables has shiny content rendered", () => {
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
            .get(".filter-option-inner-inner:first")
            .should("be.visible")
            .click()

          cy
            .get(".dropdown-menu.open li")
            .contains("Age")
            .click()
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

        cy
          .get(".irs-handle.from")
          .trigger("mousedown", { button: 0, which: 1, pageX: 600, pageY: 100 })
          .trigger("mousemove", { clientX: 0, clientY: 400 })
          .trigger("mouseup")

        cy
          .get(".irs-handle.from")
          .trigger("mousedown", { button: 0, which: 1, pageX: 600, pageY: 100 })
          .trigger("mousemove", { clientX: 0, clientY: 400 })
          .trigger("mouseup")
/*
       cy
        .get("#app-teal_wrapper-teal-main_ui-filter_panel-teal_filters_info-table table tbody tr:first")
        .then(($el, index, ))
*/
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

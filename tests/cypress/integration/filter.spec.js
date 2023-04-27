describe('Filter panes', () => {
  // Rhino applications have an 'app' prefix
  const nsPrefix = 'app-';
  // Support function to help generating namespaces
  const ns = (id) => `#${nsPrefix}${id}`;
  // Support function to help generating teal namespaces on main UI
  const nsTeal = (id) => ns(`teal_wrapper-teal-main_ui-${id}`);

  beforeEach(() => {
    cy.visit('/');

    cy
      .get('html')
      .not('.shiny-busy');

    // Find the tab and click to navigate
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .contains('KM plot for TTDE')
      .as('navPills');

    cy
      .get('@navPills')
      .click();

    cy
      .get('@navPills')
      .invoke('attr', 'href')
      .as('hrefTab');

    // Make sure the correct tab is selected
    cy.get('@hrefTab').then((hrefTab) => {
      // Look for 'Data is loaded' element
      cy.contains('Data loaded - App fully started up');

      // Define an alias that references the current active tab
      cy
        .get(`${hrefTab}.tab-pane.active`)
        .should('be.visible')
        .as('active_tab');

      // Define an alias that references the active filters
      cy
        .get(nsTeal('filter_panel-filters_overview'))
        .as('filter_summary');

      // Define an alias that references the active variables
      cy
        .get(nsTeal('filter_panel-filter_active_vars'))
        .as('filter_variables');

      // Define an alias that references the section that adds new variables to
      //  the filters
      cy
        .get(nsTeal('filter_panel-filter_add_vars'))
        .should('be.visible')
        .as('add_filter');

      // Make sure that html element does not have a class that indicates
      // that shiny is busy
      cy
        .get('html')
        .not('.shiny-busy');
    });
  });

  //  Filter summary
  // ######################################

  it('Filter Summary has non-zero Observations', () => {
    cy
      .get('@filter_summary')
      .within(() => {
        cy
          .get('.shiny-bound-output')
          .each(($el) => {
            cy
              .wrap($el)
              .children()
              .should('have.length.gte', 1);
          });
      });
  });

  //  Filter variables
  // ######################################

  it('Filter variables has shiny content rendered', () => {
    cy
      .get('@filter_variables')
      .within(() => {
        cy
          .get(nsTeal('filter_panel-filter_active_vars_contents'))
          .should('have.length.gte', 1);

        cy
          .get(nsTeal('filter_panel-ADSL_filter-filter-cards'))
          .children()
          .should('have.length', 0);

        cy
          .get(nsTeal('filter_panel-ADAS_filter-filter-cards'))
          .children()
          .should('have.length', 0);

        cy
          .get(nsTeal('filter_panel-ADTTE_filter-filter-cards'))
          .children()
          .should('have.length', 0);

        cy
          .get(nsTeal('filter_panel-ADLB_filter-filter-cards'))
          .children()
          .should('have.length', 0);
      });
  });

  //  Add variables to filter
  // ######################################

  it('Add filter variables has shiny content rendered', {
    defaultCommandTimeout: 10000,
  }, () => {
    cy
      .get('@add_filter')
      .within(() => {
        cy
          .get('.shiny-input-container')
          .each(($el) => {
            cy
              .wrap($el)
              .children()
              .should('have.length.gte', 1);
          });
      });
  });

  //  Add variables to filter
  // ######################################

  it('Add filter', () => {
    const nsInfoTable = (selector) => `${nsTeal('filter_panel-teal_filters_info-table')} ${selector}`;

    // Add Age filter (find and click on it)
    // ------------------------------------
    cy
      .get('@add_filter')
      .within(() => {
        cy
          .get('.shiny-input-container:first')
          .contains('Select variable to filter')
          .should('be.visible')
          .parent()
          .as('filterButton')

        cy.get('@filterButton', {timeout: 15000}).click('top');

        cy.get('.dropdown-menu.open li').contains('Age').as("age")

        cy.get("@age", {timeout: 15000}).click('top');
      });

    // Let shiny finish rendering
    cy
      .get('html')
      .not('.shiny-busy');

    // Test if application has non-zero subjects

    cy
      .get(nsInfoTable('table tbody tr:first td:last'))
      .contains(/[1-9]+[0-9]*\/[1-9]+[0-9]*/);

    // Monitor how many times summary is recalculated vias shiny:recalculated
    // event
    cy
      .get('@filter_summary')
      .within(() => {
        cy
          .get('.shiny-bound-output')
          .invoke('on', 'shiny:recalculated', cy.stub().as('summary_change'));
      });

    // Move slider via cypress2 mouse drag
    // ------------------------------------
    cy
      .get('@filter_variables')
      .within(() => {
        // Make sure that the overlay plot is rendered
        cy
          .get('.filterPlotOverlayRange .shiny-plot-output')
          .children()
          .should('have.length.gte', 1);

        cy
          .get('.filterPlotOverlayRange .shiny-plot-output')
          .not('.recalculating');

        // Move the handle 3x
        //  note: this is necessary as developer wasn't able to define a single
        //    long drag. This is open to improvement
        cy.get('.irs-handle.from').as('irs-handle');

        const mousedownOpts = {
          button: 0, which: 1, pageX: 600, pageY: 100,
        };
        const mousemoveOpts = {
          clientX: 300, clientY: 400,
        };

        cy.get('@irs-handle').trigger('mousedown', 'top', mousedownOpts);
        cy.get('@irs-handle').trigger('mousemove', 'top', mousemoveOpts);
        cy.get('@irs-handle').trigger('mouseup', 'top');

        cy
          .get('.filterPlotOverlayRange .shiny-plot-output')
          .not('.recalculating');
        // cy.get('@summary_change', { timeout: 20000 }).should('have.callCount', 1)

        cy.get('@irs-handle').trigger('mousedown', 'top', mousedownOpts);
        cy.get('@irs-handle').trigger('mousemove', 'top', mousemoveOpts);
        cy.get('@irs-handle').trigger('mouseup', 'top');

        cy
          .get('.filterPlotOverlayRange .shiny-plot-output')
          .not('.recalculating');

        cy.get('@irs-handle').trigger('mousedown', 'top', mousedownOpts);
        cy.get('@irs-handle').trigger('mousemove', 'top', mousemoveOpts);
        cy.get('@irs-handle').trigger('mouseup', 'top');

        cy
          .get('.filterPlotOverlayRange .shiny-plot-output')
          .not('.recalculating');
      });

    // Let shiny finish rendering
    cy
      .get('html')
      .not('.shiny-busy');

    // Verify that data is filtered (selected subjets != dataset)
    // ------------------------------------
    cy
      .get(nsInfoTable('table tbody tr:first td:last'), { timeout: 10000 })
      .should('satisfy', ($el) => {
        const result = /([0-9]+)\/([0-9]+)/.exec($el[0].innerText);
        return result.length === 3 && result[1] !== result[2];
      });
  });

  //  Active tab
  // ######################################

  it('Active tab has shiny content rendered', () => {
    cy
      .get('@active_tab')
      .within(() => {
        cy
          .get('.shiny-bound-output:visible')
          .each(($el) => {
            cy
              .wrap($el)
              .children()
              .should('have.length.gte', 1);
          });
      });
  });
});

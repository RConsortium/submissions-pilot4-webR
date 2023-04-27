describe('app', () => {
  beforeEach(() => {
    cy.visit('/');
  });

  it('Starts', () => {});

  it('Has 7 tabs', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .should('have.length', 7);
  });

  it('Navigates to all tabs', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .each(($el) => {
        cy.wrap($el).as('tealTab');

        cy.get('@tealTab').click();

        cy.get('@tealTab').invoke('attr', 'href').as('hrefTab');

        cy
          .get('@hrefTab')
          .then((hrefTab) => {
            cy
              .get(`${hrefTab}.tab-pane.active`)
              .should('be.visible')
              .then((tabPane) => {
                if (cy.wrap(tabPane).find('.shiny-bound-output').length > 0) {
                  cy.wrap(tabPane)
                    .find('.shiny-bound-output')
                    .filter(':visible')
                    .each(($el2) => {
                      cy
                        .wrap($el2)
                        .children()
                        .should('have.length.gte', 1);
                    });
                } else {
                  cy.log('Didn\'t find bound output, skipping test');
                }
              });
          });
      });
  });
});

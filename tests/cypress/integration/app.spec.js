describe('app', () => {
  beforeEach(() => {
    cy.visit('/')
  })

  it('starts', () => {})

  it('Has more than 7 tabs', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .should('have.length.at.least', 7)
  })

  it('Navigates to all tabs', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .each(($el, index, $list) => {
        cy
          .wrap($el)
          .click()
          .invoke('attr', 'href')
          .as("href_tab")

        cy.get('@href_tab').then(href_tab => {
          cy
            .get(`${href_tab}.tab-pane.active`).should('be.visible')
            .then(tab_pane => {
              if (tab_pane.find('.shiny-bound-output').length > 0) {
                cy.wrap(tab_pane)
                  .find('.shiny-bound-output')
                  .filter(':visible')
                  .each(($el2, index2, $list2)  => {
                    cy
                      .wrap($el2)
                      .children()
                      .should('have.length.gte', 1)
                  })
              } else {
                cy.log('Didn\'t find bound output, skipping test')
              }
            })
        })
      })
  })
})

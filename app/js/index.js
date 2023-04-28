Shiny.addCustomMessageHandler('toggle_dark', () => {
  document.querySelector('html > body > .color-mode').classList.toggle('dark');
});

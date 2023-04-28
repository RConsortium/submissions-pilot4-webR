/* eslint no-unused-vars: ["error", { "argsIgnorePattern": "^_" }] */
Shiny.addCustomMessageHandler('toggle_dark', (_message) => {
  document.querySelector('html > body > .color-mode').classList.toggle('dark');
});

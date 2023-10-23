var App;
Shiny.addCustomMessageHandler("toggle_dark", (function(e) {
    document.querySelector("html > body > .color-mode").classList.toggle("dark")
})), App = {};
Shiny.addCustomMessageHandler("toggle_dark", (message) => {
  console.log("toggle!!")
  document.querySelector("html > body > .color-mode").classList.toggle("dark")
});

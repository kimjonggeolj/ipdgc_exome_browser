//$(document).on("shiny:value", function(event) {
//  if (event.name === 'start') {
//    dimension = document.getElementById('forestbox').offsetWidth;
//    Shiny.onInputChange("dimension", dimension); 
//  }
//});
$(window).resize(function(event) {
    dimension = document.getElementById('geneNeedle').offsetWidth;
    Shiny.onInputChange("dimension", dimension);
});
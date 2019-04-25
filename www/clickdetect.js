function resClick(clicked_id) {
  Shiny.setInputValue('resPageId', clicked_id);
  Shiny.setInputValue('geneClick', Math.random());
}
/*
function aboutClick() {
  $('#top-row').css({
    "padding-top":"30px",
    "padding-bottom":"30px"
  });
}
*/
function varClick(clicked_id) {
  Shiny.setInputValue('varPageId', clicked_id);
  Shiny.setInputValue('varClick', Math.random());
}
/*
$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'submit') {
    aboutClick();
    //alert("aboutClick has run");
  } else {
    //alert("wrongid")
  }
});
*/
//$(document).on("shiny:value", function(event) {
  //if (event.target.id === 'mainPage') {
    //document.getElementById('resClick').addEventListener("click", resClick()
    //);
  //}
//});




function resClick(clicked_id) {
  Shiny.setInputValue('resPageId', clicked_id);
  Shiny.setInputValue('geneClick', Math.random());
}

function varResClick(clicked_id) {
  Shiny.setInputValue('varResPageId', clicked_id);
  Shiny.setInputValue('varResClick', Math.random());
}

function varClick(clicked_id) {
  Shiny.setInputValue('varPageId', clicked_id);
  Shiny.setInputValue('varClick', Math.random());
}
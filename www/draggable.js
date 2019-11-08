$(document).ready(function(){
	
	// Attach the jquery.ui draggable.
	//var containmentTop = $(".content-wrapper").position().top;
  $(".draggable").draggable({ 
		cursor: "move",
		handle: ".box-header",
        cancel: ".box-body",
        //containment: [,containmentTop,,]
  });

});
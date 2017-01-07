// Copied from: http://cssglobe.com/lab/tooltip/02/
$(document).ready(function(){
	xOffset = 10;
	yOffset = 30;

	$("a.preview").hover(function(e){
		$("body").append("<p id=\"preview\"><img src=\"" + this.href + "\" alt=\"" + this.title + "\" /></p>");
		$("#preview")
			.css("top",(e.pageY - xOffset) + "px")
			.css("left",(e.pageX + yOffset) + "px")
			.fadeIn("fast");
    },
	function(){
		$("#preview").remove();
    });

	$("a.preview").mousemove(function(e){
		$("#preview")
			.css("top",(e.pageY - xOffset) + "px")
			.css("left",(e.pageX + yOffset) + "px");
	});
});

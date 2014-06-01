$(document).ready(function() {
    $(".hidden").hide();

    $("a.toggle").click(function(event) {
        var tr = $("#" + this.id + "-tr");
        var div = $("#" + this.id + "-div");

        if (tr.is(':visible')) {
            div.hide({ effect: 'blind',
                       duration: 400,
                       complete: function(){ tr.hide(); }});
        } else {
            tr.show();
            div.show('blind', 400);
        }

        event.preventDefault();
    });
});

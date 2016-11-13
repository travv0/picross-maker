$(function() {
    $("#picrossDiv").html(newPicrossTable(10, 10));

    $(".picrossCell").click(function() { toggleCell($(this)); });
});

function newPicrossTable(width, height) {
    var picrossTable = "<table id='picrossTable'>";

    for (y = 0; y < height; ++y) {
	picrossTable += "<tr id='row" + y + "'>";

	for (x = 0; x < width; ++x) {
	    picrossTable += "<td class='picrossCell'>";
	    picrossTable += "<div id='cellContent' id='x" + x + "y" + y + "'></div>";
	    picrossTable += "</td>";
	}

	picrossTable += "</tr>";
    }

    return picrossTable;
}

function toggleCell(cell) {
    console.log("toggling");
    if (cell.hasClass("active"))
	cell.removeClass("active");
    else
	cell.addClass("active");
}

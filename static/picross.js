$(function() {
    setUpPicross(10, 10);
    $("#picrossForm").submit(function () { submitPicross($("#picrossDiv")); });
});

function setUpPicross(width, height) {
    $("#picrossDiv").html(newPicrossTable(width, height));
    $("#boardWidth").val(width);
    $("#boardHeight").val(height);
}

function newPicrossTable(width, height) {
    var picrossTable = "<table id='picrossTable'>";

    for (y = 0; y < height; ++y) {
	picrossTable += "<tr id='row" + y + "'>";

	for (x = 0; x < width; ++x) {
	    picrossTable += "<td onclick='toggleCell($(this))' class='picrossCell' id='x" + x + "y" + y + "'>";
	    picrossTable += "</td>";
	}

	picrossTable += "</tr>";
    }

    return picrossTable;
}

function toggleCell(cell) {
    if (cell.hasClass("active"))
	cell.removeClass("active");
    else
	cell.addClass("active");
}

function submitPicross(picross) {
    $("#picrossList").val(makePicrossList(picross));

    return true;
}

function makePicrossList(picross) {
    var picrossList = "";

    $("#picrossTable tr").each(function() {
	$('td', this).each(function() {
	    if ($(this).hasClass("active")) {
		if (picrossList !== "")
		    picrossList += ",";
		picrossList += $(this)[0].id;
	    }
	});
    });

    return picrossList;
}

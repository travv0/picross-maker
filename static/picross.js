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
	    picrossTable += "<td onclick='playCell($(this))' class='picrossCell' id='x" + x + "y" + y + "'>";
	    picrossTable += "</td>";
	}

	picrossTable += "</tr>";
    }

    return picrossTable;
}

function toggleCell(cell) {
    if ($("#mode").val() === "mark")
	markCell(cell);
    else
	playIfCorrect(cell);
}

function playCell(cell) {
    if (cell.hasClass("active"))
	cell.removeClass("active");
    else
	cell.addClass("active");
}

function markCell(cell) {
    if (cell.hasClass("marked")) {
	cell.removeClass("marked");
	$("div", cell).text("");
    }
    else {
	cell.addClass("marked");
	$("div", cell).text("X");
    }
}

function givePenalty() {
    $("#penaltyCounter").text(parseInt($("#penaltyCounter").text()) + 1);
}

function playIfCorrect(cell) {
    $.ajax({
	url: "/check-cell?id=" + getParameterByName("id") + "&cell=" + cell.attr("id"),
	success: function(data) {
	    console.log(data);
	    if (data == 1) {
		console.log("playing cell " + cell);
		playCell(cell);
	    }
	    else {
		console.log("marking cell " + cell);
		givePenalty();
		if (!cell.hasClass("marked"))
		    markCell(cell);
	    }
	}
    });
}

function submitPicross(picross) {
    $("#picrossList").val(makePicrossList(picross));

    return $("#picrossList").val() !== "";
}

function submitSolution(picross) {
    $.get("/submit-solution?id=" + getParameterByName("id") + "&cells=" + makePicrossList(picross),
	  function(data) {
	      alert(data);
	  });
}

function getParameterByName(name, url) {
    if (!url) {
	url = window.location.href;
    }
    name = name.replace(/[\[\]]/g, "\\$&");
    var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
    results = regex.exec(url);
    if (!results) return null;
    if (!results[2]) return '';
    return decodeURIComponent(results[2].replace(/\+/g, " "));
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

function updatePicrossTable() {
    $("#picrossDiv").html(newPicrossTable(parseInt($("#boardWidth").val()),
					  parseInt($("#boardHeight").val())));
}

function toggleMode() {
    if ($("#mode").val() === "play") {
	$("#mode").val("mark");
	$("#modeLink").text("switch to play mode");
    }
    else {
	$("#mode").val("play");
	$("#modeLink").text("switch to mark mode");
    }
}

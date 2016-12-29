"use strict";

document.addEventListener("DOMContentLoaded", function() {
	var i;
	var times = document.querySelectorAll(".time");
	for (i = 0; i < times.length; i++) {
		var date = new Date(times[i].innerHTML * 1000);
		times[i].innerHTML = date.toLocaleString();
	}

	var clicking = false;
	var mode = '';
	var cells = document.querySelectorAll(".picrossCell.solve");
	for (i = 0; i < cells.length; i++) {
		var cell = cells[i];
		cell.onmousedown = function(e) {
			if (!mode) {
				if (cell.classList.contains('marked') ||
				    cell.classList.contains('active'))
					mode = 'erase';
				else if (document.getElementById('mode') === 'mark' ||
					 e.shiftKey)
					mode = 'mark';
				else mode = 'play';
				console.log(mode);
			}
			clicking = true;
			if (mode === 'mark' &&
			    !cell.classList.contains('active') &&
			    !cell.classList.contains('marked'))
				alert("markin"); //markCell(cell);
			else if (mode === 'play' &&
				 !cell.classList.contains('active') &&
				 !cell.classList.contains('marked'))
				alert("togglin"); //toggleCell(cell);
			else if (mode === 'erase')
				alert("erasin"); //eraseCell(cell;
		};
	};

		// .mousemove(function(e) {
		//	if (clicking) {
		//		if (mode === 'mark' &&
		//		    !$(this).hasClass('active') &&
		//		    !$(this).hasClass('marked'))
		//			markCell($(this));
		//		else if (mode === 'play' &&
		//			 !$(this).hasClass('active') &&
		//			 !$(this).hasClass('marked'))
		//			toggleCell($(this));
		//		else if (mode === 'erase')
		//			eraseCell($(this));
		//	}
		// })
		// .mouseup(function() {
		//	clicking = false;
		//	mode = '';
		// });
});

function setUpPicross(width, height) {
	document.getElementById("picrossDiv").innerHTML = newPicrossTable(width, height);
	// $("#boardWidth").val(width);
	// $("#boardHeight").val(height);
}

function newPicrossTable(width, height) {
	var picrossTable = "<table id='picrossTable'>";

	for (var y = 0; y < height; ++y) {
		picrossTable += "<tr id='row" + y + "'>";

		for (var x = 0; x < width; ++x) {
			picrossTable += "<td class='picrossCell' id='x" + x + "y" + y + "'>";
			picrossTable += "</td>";
		}

		picrossTable += "</tr>";
	}

	return picrossTable;
}

function toggleCell(cell) {
	cell.addClass("active").addClass("pending");
	playIfCorrect(cell);
}

function playCell(cell) {
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

function eraseCell(cell) {
	$("div", cell).text("");
	cell.removeClass('active').removeClass('marked');
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
				cell.removeClass('pending');
				playCell(cell);
			}
			else {
				console.log("marking cell " + cell);
				cell.removeClass('pending').removeClass('active');
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

function submitLogin() {
	if ($('#password').val() !== '' && $('#username').val() !== '') {
		$('#password').val(Sha1.hash($('#password').val()));
		$('#loginForm').submit();
	}
}

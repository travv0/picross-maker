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
		var filled = cell.classList.contains('marked') ||
			    cell.classList.contains('active');

		cell.onmousedown = function(e) {
			if (!mode) {
				if (filled)
					mode = 'erase';
				else if (document.getElementById('mode') === 'mark' ||
					 e.shiftKey)
					mode = 'mark';
				else mode = 'play';
				console.log(mode);
			}
			clicking = true;
			if (mode === 'mark' && !filled)
				markCell(cell);
			else if (mode === 'play' && !filled)
				playCell(cell);
			else if (mode === 'erase')
				eraseCell(cell);
		};

		cell.onmousemove = function(e) {
			if (clicking) {
				if (mode === 'mark' && !filled)
					markCell(cell);
				else if (mode === 'play' && !filled)
					playCell(cell);
				else if (mode === 'erase')
					eraseCell(cell);
			}
		};

		cell.onmouseup = function() {
			console.log("mouse-upin");
			clicking = false;
			mode = '';
			console.log(mode);
		};
	}
});

function setUpPicross(width, height) {
	document.getElementById("picrossDiv").innerHTML = newPicrossTable(width, height);
	document.getElementById("boardWidth").value = width;
	document.getElementById("boardHeight").value = height;
}

function newPicrossTable(width, height) {
	var picrossTable = "<table id='picrossTable'>";

	for (var y = 0; y < height; ++y) {
		picrossTable += "<tr id='row" + y + "'>";

		for (var x = 0; x < width; ++x) {
			picrossTable += "<td class='picrossCell' onclick='toggleCell(this)' id='x" + x + "y" + y + "'>";
			picrossTable += "</td>";
		}

		picrossTable += "</tr>";
	}

	return picrossTable;
}

function playCell(cell) {
	cell.classList.add("active", "pending");
	playIfCorrect(cell);
}

function toggleCell(cell) {
	cell.classList.toggle("active");
}

function markCell(cell) {
	if (cell.classList.contains("marked")) {
		cell.firstChild.innerHTML = "";
	}
	else {
		cell.firstChild.innerHTML = "X";
	}
	cell.classList.toggle("marked");
}

function eraseCell(cell) {
	cell.firstChild.innerHTML = "";
	cell.classList.remove('active', 'marked');
}

function givePenalty() {
	var penaltyCounter = document.getElementById("penaltyCounter");
	penaltyCounter.innerHTML = parseInt(penaltyCounter.innerHTML) + 1;
}

function playIfCorrect(cell) {
	cell.classList.remove("pending");
	// $.ajax({
	//	url: "/check-cell?id=" + getParameterByName("id") + "&cell=" + cell.attr("id"),
	//	success: function(data) {
	//		console.log(data);
	//		if (data == 1) {
	//			console.log("playing cell " + cell);
	//			cell.removeClass('pending');
	//			playCell(cell);
	//		}
	//		else {
	//			console.log("marking cell " + cell);
	//			cell.removeClass('pending').removeClass('active');
	//			givePenalty();
	//			if (!cell.hasClass("marked"))
	//				markCell(cell);
	//		}
	//	}
	// });
}

function submitPicross(picross) {
	var picrossList = document.getElementById("picrossList");
	picrossList.value = makePicrossList(picross);

	return picrossList.value !== "";
}

function submitSolution(picross) {
	// $.get("/submit-solution?id=" + getParameterByName("id") + "&cells=" + makePicrossList(picross),
	//       function(data) {
	//	      alert(data);
	//       });
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
	var picrossRows = document.querySelectorAll("#picrossTable tr");

	for (var i = 0; i < picrossRows.length; i++) {
		var row = picrossRows[i];
		var picrossCols = row.childNodes;

		for (var j = 0; j < picrossCols.length; j++) {
			var col = picrossCols[j];
			console.log(col[0]);
			if (col.classList.contains("active")) {
				if (picrossList !== "")
					picrossList += ",";
				picrossList += col.id;
			}
		};
	};

	return picrossList;
}

function updatePicrossTable() {
	document.getElementById("picrossDiv").innerHTML =
		newPicrossTable(parseInt(document.getElementById("boardWidth").value),
				parseInt(document.getElementById("boardHeight").value));
}

function toggleMode() {
	var modeLink = document.getElementById("modeLink");
	if (document.getElementById("mode").value === "play") {
		document.getElementById("mode").value = "mark";
		modeLink.innerHTML = "switch to play mode";
	}
	else {
		document.getElementById("mode").value = "play";
		modeLink.innerHTML = "switch to mark mode";
	}
}

function submitLogin(form) {
	if (document.getElementById('password').value !== '' &&
	    document.getElementById('username').value !== '')
		form.submit();
}

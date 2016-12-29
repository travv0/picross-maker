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
            var filled = this.classList.contains('marked') ||
                    this.classList.contains('active');
            if (!mode) {
                if (filled)
                    mode = 'erase';
                else if (document.getElementById('mode') === 'mark' ||
                         e.shiftKey)
                    mode = 'mark';
                else mode = 'play';
            }
            clicking = true;
            if (mode === 'mark' && !filled)
                markCell(this);
            else if (mode === 'play' && !filled)
                playCell(this);
            else if (mode === 'erase')
                eraseCell(this);
        };

        cell.onmousemove = function(e) {
            var filled = this.classList.contains('marked') ||
                    this.classList.contains('active');
            if (clicking) {
                if (mode === 'mark' && !filled)
                    markCell(this);
                else if (mode === 'play' && !filled)
                    playCell(this);
                else if (mode === 'erase')
                    eraseCell(this);
            }
        };

        cell.onmouseup = function() {
            clicking = false;
            mode = '';
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
    if (!cell.classList.contains("solve"))
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

    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
            if (xmlhttp.status == 200) {
                var data = xmlhttp.responseText;
                if (data == 1) {
                    cell.classList.remove('pending');
                }
                else {
                    cell.classList.remove('pending', 'active');
                    givePenalty();
                    if (!cell.classList.contains("marked"))
                        markCell(cell);
                }
            }
            else
                console.log("Error checking move.");
        }
    };

    xmlhttp.open("GET", "check-cell?id=" + getParameterByName("id") +
                 "&cell=" + cell.id, true);
    xmlhttp.send();
}

function submitPicross() {
    var picrossList = document.getElementById("picrossList");
    picrossList.value = makePicrossList(document.getElementById("picrossDiv"));

    return picrossList.value !== "";
}

function submitSolution(picross) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState == XMLHttpRequest.DONE) {
            if (xmlhttp.status == 200) {
                var data = xmlhttp.responseText;
                alert(data);
            }
            else
                console.log("Error submitting solution.");
        }
    };

    xmlhttp.open("GET", "/submit-solution?id=" + getParameterByName("id") +
                 "&cells=" + makePicrossList(picross), true);
    xmlhttp.send();
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

"use strict";

/*
  Removes all children from an element
*/
function removeChildren(element) {
    Array.from(element.children).map((c) => c.remove());
}

/*
  Adds the error class and red background to one element
*/
function markError(element) {
    element.classList.add("error");
}

/*
  Removes the error class and red background from one element
*/
function resetError(element) {
    element.classList.remove("error");
}

/*
  Adds a new error message in red to the error div
*/
function addErrorMessage(message) {
    var errorDiv = document.getElementById("errorDiv"),
        errorP = document.createElement("p");
    errorP.classList.add("errorMsg");
    errorP.append(message);
    errorDiv.append(errorP);
}

/*
  Remove all error messages from the error div
 */
function removeErrorMessages() {
    removeChildren(document.getElementById("errorDiv"));
}

/*
  Removes the error class and red background from all elements in the class
 */
function resetAllErrors() {
    for (const e of Array.from(document.getElementsByClassName("error"))) {
	resetError(e);
    }
}

/*
  Checks if the query string is valid and adds error message and highlights culprit

  Returns false in case of an error and true otherwise
 */
async function checkQuery(queryElement) {
    var error = false;
    await fetch("/check_query?query=" + queryElement.value)
	.then((response) => response.json())
	.then((data) => {
	    if (data.status != "valid") {
		markError(queryElement);
		addErrorMessage("Problem with query: " + data.msg);
		error = true
	    }
	});
    return !error;
}

/*
  Checks if the replacement string is valid and adds error message and highlights culprit

  Returns false in case of an error and true otherwise
 */
async function checkReplacement(replacementElement) {
    var error = false;
    await fetch("/check_replacement?replacement=" + replacementElement.value)
	.then((response) => response.json())
	.then((data) => {
	    if (data.status != "valid") {
		markError(replacementElement);
		addErrorMessage("Problem with replacement: " + data.msg);
		error= true;
	    }
	});
    return !error;
}

/*
  Creates the link to a temporary file.

  Returns a new <a> element
*/
function createTmpLink(file, text) {
    var link = document.createElement("a");
    link.href = "/tmp_file?filename=" + file;
    link.text = text;
    link.target = "_blank";
    link.classList = "tmpLink";
    return link;
}

/*
  Creates a new line in the result table, i.e. a div with two spans

  Returns a new <div> element
*/
function createLine(leftField,rightField) {
    var lineDiv = document.createElement("div"),
	leftSpan = document.createElement("span"),
	rightSpan = document.createElement("span");
    leftSpan.classList.add("resultCell")
    rightSpan.classList.add("resultCell")
    leftSpan.innerHTML=leftField;
    rightSpan.innerHTML=rightField;
    lineDiv.append(leftSpan);
    lineDiv.append(rightSpan);
    return lineDiv;
}

/*
  Sends the form data to the server and updates the user interface based on the result.
*/
async function sendData() {
    // Remove all previous errors
    removeErrorMessages();
    resetAllErrors();
    var error = false;
    var l2treebank = document.getElementById("l2treebank").value;
    if (l2treebank == "") {
	markError(document.getElementById("l2span"));
	addErrorMessage("L2 treebank is required");
	error = true;
    }
    var queryElement = document.getElementById("query");
    // Replace empty query by the default value
    if (queryElement.value == "") {
	queryElement.value="DEPREL_ \"root\"";
    }
    // Checks the query
    var result = await checkQuery(queryElement);
    error = error || !result
    var replacementElement = document.getElementById("replacement");
    if (replacementElement.value != "") {
	// Get the 
	var result = await checkReplacement(replacementElement)
	error = error || !result	
    }
    if (!error) {
	// Get the form data
	var formData = new FormData(document.getElementById("searchForm"));
	// Show overlay
	showOverlay();
	// Send the request. Because we "await" the fetch, this will block
	const response = await fetch("/search_treebanks", {
	    method: "POST",
	    body: formData,
	}).then((response) => response.json());
	// Update the page with the results
	document.getElementById("hitsSpan").textContent = response.l1.length + " hits";
	var downloadsSpan = document.getElementById("downloadsSpan");
	// Cleanup old download links
	removeChildren(downloadsSpan);
	// Add new download links
	downloadsSpan.append(createTmpLink(response.l1file, "L1 file"));
	downloadsSpan.append(createTmpLink(response.l2file, "L2 file"));
	downloadsSpan.append(createTmpLink(response.l1l2file, "L1-L2 file"));
	var resultsDiv = document.getElementById("resultsDiv");
	// Cleanup old results
	removeChildren(resultsDiv);
	// Set font depending on mode
	if (document.getElementById("conllMode").checked) {
	    resultsDiv.style.fontFamily="monospace,monospace";
	}
	else {
	    resultsDiv.style.fontFamily="inherit";
	}
	// Store the filenames in the hidden fields of the form
	document.getElementById("l1file").value = response.l1file;
	document.getElementById("l2file").value = response.l2file;
	document.getElementById("l1l2file").value = response.l1l2file;
	// Display all the results
	for (var index = 0; index < response.l1.length; index++) {
	    resultsDiv.append(createLine(response.l1[index],response.l2[index]));
	}
	// Hide the overlay when we are done
	hideOverlay();
    }
}

/*
  Shows the overlay while the process in operation
*/
function showOverlay() {
    document.getElementById("overlay").style.display = "block";
}

/*
  Hides the overlay again
*/
function hideOverlay() {
    document.getElementById("overlay").style.display = "none";
}

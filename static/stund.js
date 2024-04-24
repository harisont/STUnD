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
    var errordiv = document.getElementById("errorDiv");
    var errorp = document.createElement("p");
    errorp.append(message);
    errordiv.append(errorp);
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
    for (const e of document.getElementsByClassName("error")) {
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
	var formData = new FormData(document.getElementById("searchform"));
	const response = await fetch("/search_treebanks", {
	    method: "POST",
	    body: formData,
	}).then((response) => response.json());
	// Update the page with the results
	document.getElementById("hitsSpan").textContent = response.l1.length + " hits";
	var downloadsspan = document.getElementById("downloadsSpan");
	// Cleanup old downloads
	removeChildren(downloadsSpan);
	downloadsSpan.append(createTmpLink(response.l1file, "L1 file"));
	downloadsSpan.append(createTmpLink(response.l2file, "L2 file"));
	downloadsSpan.append(createTmpLink(response.l1l2file, "L1-L2 file"));
	// Cleanup old results
	removeChildren(resultsDiv);
	if (document.getElementById("conllMode").checked) {
	    document.getElementById("l1resultSpan").style.fontFamily="monospace,monospace";
	    document.getElementById("l2resultSpan").style.fontFamily="monospace,monospace";
	}
	else {
	    document.getElementById("l1resultSpan").style.fontFamily="inherit";
	    document.getElementById("l2resultSpan").style.fontFamily="inherit";
	}
	var l2result = ""
	for (line of response.l2) {
	    l2result = l2result + "<p>" + line + "</p>";
	}
	document.getElementById("l2resultSpan").innerHTML = l2result;
	var l1result = ""
	for (line of response.l1) {
	    l1result = l1result + "<p>" + line + "</p>";
	}
	document.getElementById("l1resultSpan").innerHTML = l1result;
    }
}

"use strict";

/*
  Function to be called when the page is loaded
*/
function pageLoad() {
    // Load saved queries from local storage
    loadFromStore("queries");
    // Load saved queries from local storage
    loadFromStore("replacements");
}

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
    await fetch("../check_query?query=" + queryElement.value)
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
    await fetch("../check_replacement?replacement=" + replacementElement.value)
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
  Checks if the conll file is valid and adds error message and highlights culprit

  Returns false in case of an error and true otherwise
 */
async function checkConll(treebankElement,checkedTreebankElement) {
    var error = false;
    if (checkedTreebankElement.value=="false") {
	var formData = new FormData();
	formData.set(treebankElement.id,treebankElement.files[0]);
	await fetch("../check_conll", {
	    method: "POST",
	    body: formData,
	})
	.then((response) => response.json())
	.then((data) => {
	    if (data.status != "valid") {
		markError(treebankElement);
		addErrorMessage("Problem with treebank file: " + data.msg);
		for (error of data.parsesOrErrors) {
		    addErrorMessage(error);
		}
		error= true;
	    }
	    else {
		checkedTreebankElement.value="true";
	    }
	});
    }
    return !error;
}

/*
  Creates the link to a temporary file.

  Returns a new <a> element
*/
function createTmpLink(file, text) {
    var link = document.createElement("a");
    link.href = "../tmp_file?filename=" + file;
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
    lineDiv.classList.add("resultRow")
    leftSpan.classList.add("resultCell")
    leftSpan.innerHTML=leftField;
    lineDiv.append(leftSpan);
    if (rightField != undefined) {
	rightSpan.classList.add("resultCell")
	rightSpan.innerHTML=rightField;
	lineDiv.append(rightSpan);
    }
    else {
	leftSpan.style["width"]="98%";
    }
    return lineDiv;
}

/*
  Handle fetch errors by updating the overlay
*/
function handleFetchError(error) {
    document.getElementById("overlay").style.background="red";
    var icon = document.getElementById("icon");
    icon.style.animation="Initial";
    icon.style.transform="scaleY(-1)";
    var overlayContent = document.getElementById("overlayContent");
    overlayContent.append("Something went wrong :(")
    overlayContent.append(document.createElement("br"));
    overlayContent.append(error.message);
}

/*
  Stores the query in the local storage
*/
function saveToStore(category, value) {
    // Get queries from local storage
    var lists = JSON.parse(localStorage.savedLists);
    // Initialize new list if category is missing
    if (!(category in lists)) {
	lists[category]= []
    }
    // Add if not yet in the list
    if (!lists[category].find((q) => value === q)) {
	lists[category].unshift(value)
	// Also add to query list
	var option = document.createElement("option");
	option.value=value;
	document.getElementById(category).prepend(option);
    }
    // Store in local storage again
    localStorage.savedLists = JSON.stringify(lists);
}

/*
  Load stored lists from local storage
*/
function loadFromStore(category) {
    // Load queries from storage and add to list
    if (localStorage.savedLists) {
	var lists = JSON.parse(localStorage.savedLists)
	if (category in lists) {
	    var listElement = document.getElementById(category);
	    for (const opt of lists[category]) {
		var tmpOption = document.createElement("option");
		tmpOption.value=opt;
		listElement.append(tmpOption);
	    }
	}
    }
    // Otherwise initialize storage
    else {
	localStorage.savedLists = JSON.stringify({});
    }
}


/*
  Send form on press of the return key
*/
function handleReturnKey(e) {
    if(e && e.keyCode == 13) {
	sendData();
    }
}

/*
  Sends the form data to the server and updates the user interface based on the result.
*/
async function sendData() {
    // Show overlay
    showOverlay();
    // Remove all previous errors
    removeErrorMessages();
    resetAllErrors();
    var error = false;
    // Check if the required treebank is present
    var treebank1 = document.getElementById("treebank1");
    if (treebank1.value == "") {
	markError(document.getElementById("t1span"));
	addErrorMessage("Treebank 1 is required");
	error = true;
    }
    // Check the Conll files
    var result = await checkConll(l1treebank,document.getElementById("checkedL1Treebank"));
    error = error || !result;
    var l2treebank = document.getElementById("l2treebank");
    if (l2treebank.value != "") {
	var result = await checkConll(l2treebank,document.getElementById("checkedL2Treebank"));
	error = error || !result
    }
    // Checks the query
    var queryElement = document.getElementById("query");
    // Replace empty query by the default value
    if (queryElement.value == "") {
	queryElement.value="DEPREL_ \"root\"";
    }
    var result = await checkQuery(queryElement, document.getElementById("parsedQuery"));
    error = error || !result
    // Checks the replacement
    var replacementElement = document.getElementById("replacement");
    if (replacementElement.value != "") {
	var result = await checkReplacement(replacementElement,document.getElementById("parsedReplacement"))
	error = error || !result	
    }
    if (!error) {
	// Store the query in the local storage and list of queries
	saveToStore("queries", document.getElementById("query").value);
	let replacement = document.getElementById("replacement").value;
	if (replacement != "")
	    saveToStore("replacements", replacement);
	// Get the form data
	var formData = new FormData(document.getElementById("searchForm"));
	// Remove unused data before sending it
	formData.delete("checkedTreebank1");
	formData.delete("checkedTreebank2");
	// Send the request. Because we "await" the fetch, this will block
	const response = await fetch("../search_treebanks", {
	    method: "POST",
	    body: formData,
	})
	.then((response) => {
	    if (!response.ok) {
		throw new Error(`HTTP error ${response.status}: ${response.statusText}`);
	    }
	    return response.json()
	})
	.catch((error) => handleFetchError(error));
	// Update the page with the results
	document.getElementById("hitsSpan").textContent = response.t1.length + " hits";
	var downloadsSpan = document.getElementById("downloadsSpan");
	// Cleanup old download links
	removeChildren(downloadsSpan);
	// Add new download links and store the file names for later reuse in hidden fields of the form
	var saveSpan = document.createElement("span")
	saveSpan.textContent = "- save: "
	downloadsSpan.append(saveSpan)
	downloadsSpan.append(createTmpLink(response.t1file, "T1 file"));
	document.getElementById("t1file").value = response.t1file;
	if (response.t2file != null) {
	    downloadsSpan.append(createTmpLink(response.t2file, "T2 file"));
	    document.getElementById("t2file").value = response.t2file;
	}
	if (response.t1t2file != null) {
	    downloadsSpan.append(createTmpLink(response.t1t2file, "parallel file"));
	    document.getElementById("t1t2file").value = response.t1t2file;
	}
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
	// Display all the results
	for (var index = 0; index < response.t1.length; index++) {
	    resultsDiv.append(createLine(response.t1[index],response.t2[index]));
	}
    }
    // Hide the overlay when we are done
    hideOverlay();
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

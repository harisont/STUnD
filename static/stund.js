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
  Removes all children from an element matching a filter criterion
*/
function removeChildren(element, filterCriterion = ((e) => true)) {
	Array.from(element.children).filter(filterCriterion).map((c) => c.remove());
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
  Makes all fields in a CONLL treebank editable an manage button stuff
*/
function makeEditable(treebank) {
    var checkbox = document.getElementById("t" + treebank + "editableBox");
    // Check if we are in an editing state
    if (checkbox.checked) {
	// Update all result cells
	for (const e of Array.from(document.getElementsByClassName("t" + treebank + "resultCell"))) {
	    // Enable editing by adding the contenteditable attribute	    
	    e.setAttribute("contenteditable","");
	    e.setAttribute("oninput","editedTreebank('" + treebank + "')");
	}
    }
    else {
	// Update all result cells
	for (const e of Array.from(document.getElementsByClassName("t" + treebank + "resultCell"))) {
	    // Disable editing by removing the contenteditable attribute again
	    e.removeAttribute("contenteditable")
	}
    }
}

/*
  To be called when a treebank is edited
*/
function editedTreebank(treebank) {
    // Enable resubmit
    document.getElementById("t" + treebank + "resubmit").removeAttribute("disabled");
    // Set the edited flag
    document.getElementById("editedTreebank" + treebank).value="true";
    document.getElementById("checkedTreebank" + treebank).value="false";
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
 false in case of an error and true otherwise
 */
async function checkConll(treebankFile,checkedTreebankElement) {
    var error = false;
    if (checkedTreebankElement.value=="false") {
	var formData = new FormData();
	formData.set(treebankFile.name,treebankFile);
	await fetch("../check_conll", {
	    method: "POST",
	    body: formData,
	})
	.then((response) => response.json())
	.then((data) => {
	    if (data.status != "valid") {
		// For both file input elements, find the one which caused the problem. It can only be either the one or the other.
		if (document.getElementById("treebank1").value.endsWith(treebankFile.name)) {
		    markError(document.getElementById("treebank1"))
		}
		else {
		    markError(document.getElementById("treebank2"))
		}
		addErrorMessage("Problem with treebank file " + treebankFile.name + ": " + data.msg);
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
function createLine(n,leftField,leftHighlight,rightField,rightHighlight) {
    var lineDiv = document.createElement("div"),
	leftSpan = document.createElement("span"),
	rightSpan = document.createElement("span");
    lineDiv.classList.add("resultRow")
    leftSpan.classList.add("resultCell")
    leftSpan.classList.add("t1resultCell")
	var leftNumDiv = document.createElement("div"),
	leftTxtDiv = document.createElement("div"),
	rightNumDiv = document.createElement("div"),
	rightTxtDiv = document.createElement("div");
    leftNumDiv.classList.add("numdiv", "align-left", "unselectable");
    rightNumDiv.classList.add("numdiv","align-right", "unselectable");
    leftTxtDiv.classList.add("txtdiv");
    rightTxtDiv.classList.add("txtdiv");
    leftSpan.appendChild(leftNumDiv);
    leftSpan.appendChild(leftTxtDiv);
    rightSpan.appendChild(rightTxtDiv);
    rightSpan.appendChild(rightNumDiv);
    leftNumDiv.innerHTML=n;
    leftTxtDiv.innerHTML=leftField;
    highlight(leftTxtDiv,leftHighlight)
    lineDiv.append(leftSpan);
    if (rightField != undefined) {
	rightSpan.classList.add("resultCell")
	rightSpan.classList.add("t2resultCell")
	rightTxtDiv.innerHTML=rightField;
	highlight(rightTxtDiv,rightHighlight)
    rightNumDiv.innerHTML=n;
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
  Reset the edited flags and disable buttons
*/
function resetEditable() {

    document.getElementById("editedTreebank1").value = "false";
    document.getElementById("editedTreebank2").value = "false";
    document.getElementById("t1editableBox").checked = false;
    document.getElementById("t2editableBox").checked = false;
    document.getElementById("t1resubmit").setAttribute("disabled", "");
    document.getElementById("t2resubmit").setAttribute("disabled", "");
}

async function sendFiles() {
    var treebank1 = document.getElementById("treebank1");
    // Get edited flags
    var isEdited = document.getElementById("editedTreebank1").value == "true" || document.getElementById("editedTreebank1").value == "true";
    if (isEdited) {
	if (!window.confirm("The data has been modified. The changes will be discarded if you continue now. Are you sure?")) {
	    // Cancel on user input
	    return;
	}
    }
    var treebank2 = document.getElementById("l2treebank");
    // Also get the second treebank
    if (treebank1.value.endsWith(".txt") || (treebank2 != null && treebank2.value.endsWith(".txt"))) {
	parseAndSendFiles();
    }
    else {
	var formData = new FormData(document.getElementById("searchForm"));
	queryData(formData);
	resetEditable();
    }
}

/*
  Parse a single plaintext file into a treebank using UDPipe and identifies the language if necessary
 */
async function parsePlaintext(treebank) {
    // Try to determine the language
    var lang = "";
    // name starts with XX_ where XX is a language code => we are done
    if (treebank.name.match("^[a-z]{2}_")) {
	lang = treebank.name.substr(0,2);
    }
    // We have to guess the language
    else {
	// Use langid.js (https://github.com/saffsd/langid.js) to identify the language
	showOverlay("identifying language...");
	lang = langid.identify(await treebank.text());
	hideOverlay();
    }
    // If we got a language we can use UDPipe for processing
    if (lang != "") {
	
	// Set parameters for UDPipe
	var udopipeData = new FormData();
	udopipeData.set("model", lang);
	udopipeData.set("tokenizer", "");
	udopipeData.set("tagger", "");
	udopipeData.set("parser", "");
	udopipeData.set("data", await treebank.text());
	showOverlay("parsing via UDPipe...");
	var treebankData = await fetch("https://lindat.mff.cuni.cz/services/udpipe/api/process", {
	    method: "POST",
	    body: udopipeData,
	})
	    .then((response) => {
		hideOverlay();
		// Check response status
		if (response.status != 200) {
		    // Create error message
		    response.text().then((txt) => {
			var msg = "Error with UDPipe: " + txt;
			addErrorMessage(msg);
		    });
		    // Create fake empty JSON
		    return {"result": ""};
		}
		else {
		    // If no problem with parsing, return as JSON
		    return response.json();
		}
	    });
	return treebankData.result;
    }	
}

/*
  Parse plaintext files into treebanks before submitting thems
 */
async function parseAndSendFiles() {
    var formData = new FormData(document.getElementById("searchForm"));
    var treebank1 = formData.get("treebank1");
    var treebank2 = formData.get("treebank2");
    var formData = new FormData(document.getElementById("searchForm"));
    // Ask user before sending data to external service
    if (!window.confirm("Your data will be sent to an external service for processing. Is that okay?")) {
	// Cancel on user input
	return;
    }
    var emptyTreebank = false;
    // Update the treebanks
    if (treebank1.name.endsWith("txt")) {
	var treebankData = await parsePlaintext(treebank1);
	if (treebankData != "") {
	    formData.delete("treebank1"); 
	    formData.set("treebank1", new File([treebankData],"treebank1udpipe.conllu"))
	}
	else {
	    emptyTreebank = true;
	}
    }
    if (treebank2.name.endsWith("txt")) {
	var treebankData = await parsePlaintext(treebank2);
	if (treebankData != "") {
	    formData.delete("treebank2"); 
	    formData.set("treebank2", new File([treebankData],"treebank2udpipe.conllu"))
	}
	else {
	    emptyTreebank = true;
	}
    }
    if (!emptyTreebank) {
	queryData(formData);
    }
}

async function resendEditedData() {
    var formData = new FormData(document.getElementById("searchForm"));
    // Read the treebanks from the HTML table
    var newTreebank1 = []
    var newTreebank2 = []
    for (const e of Array.from(document.getElementsByClassName("t1resultCell"))) {
	newTreebank1.push(e.textContent);
    }
    for (const e of Array.from(document.getElementsByClassName("t2resultCell"))) {
	newTreebank2.push(e.textContent);
    }
    // Update the treebanks
    formData.delete("treebank1");
    formData.set("treebank1", new File(newTreebank1,"treebank1tmp.conllu"))
    formData.delete("treebank2");
    formData.set("treebank2", new File(newTreebank2,"treebank2tmp.conllu"))
    queryData(formData);
    resetEditable();
}

/*
  Sends the form data to the server and updates the user interface based on the result.
*/
async function queryData(formData) {
    var error = false;
    // Show overlay
    showOverlay("validating input and running query...");
    // Remove all previous errors
    removeErrorMessages();
    resetAllErrors();
    // Check if the required treebank is present
    if (formData.get("treebank1").name == "") {
	markError(document.getElementById("t1span"));
	addErrorMessage("Treebank 1 is required");
	error = true;
    }
    // Check the Conll files
    var result = await checkConll(formData.get("treebank1"),document.getElementById("checkedTreebank1"));
    error = error || !result;
    if (formData.get("treebank2").name != "") {
	var result = await checkConll(formData.get("treebank2"),document.getElementById("checkedTreebank2"));
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
	removeChildren(resultsDiv, ((e) => e.getAttribute("class") == "resultRow"));
	// Set font depending on mode
	if (document.getElementById("conllMode").checked) {
	    resultsDiv.style.fontFamily="monospace,monospace";
	    // Show the "editable" checkbox
	    document.getElementById("t1editableSpan").style.setProperty("display", "inline-block");
            document.getElementById("t1editableSpan").style.setProperty("font-family", "sans-serif");
	    if (!(response.t2[0] == undefined)) {
		document.getElementById("t2editableSpan").style.setProperty("display", "inline-block");
		document.getElementById("t2editableSpan").style.setProperty("font-family", "sans-serif");
	    }
	}
	else {
	    resultsDiv.style.fontFamily="inherit";
	    // Hide "editable" checkboxes
	    document.getElementById("t1editableSpan").style.setProperty("display", "none");
	    document.getElementById("t2editableSpan").style.setProperty("display", "none");
	}
	// Display all the results
	for (var index = 0; index < response.t1.length; index++) {
	    resultsDiv.append(createLine(index + 1, response.t1[index], response.h1[index], response.t2[index],response.h2[index]));
	}
    }
    // Hide the overlay when we are done
    hideOverlay();
}


/*
  Highlights divergences in the results
*/
function highlight(element, indices) {
    const indexSet = Array.from(new Set(indices))
    if (document.getElementById("textMode").checked) {
	const boldNodes = Array.from(element.getElementsByTagName("b"))
	for (const i in indexSet) {
	    const child = boldNodes[indexSet[i]-1]
	    var mark = document.createElement("span")
	    mark.className = "mark"
	    child.parentElement.replaceChild(mark,child)
	    mark.innerHTML = child.outerHTML
	}
    }
    else if (document.getElementById("conllMode").checked) {
	var lines = element.innerHTML.split("\n")
	for (const i in indexSet) {
	    lines[indexSet[i]-1]="<span class=\"mark\">" + lines[indexSet[i]-1] + "</span>"
	}
	element.innerHTML = lines.join("\n")
    }
    else { // Tree mode
	for (const i in indexSet) {
	    element.getElementsByTagName("text")[indexSet[i]-1].style.fill = "#9449D1"
	}
    }
}

/*
  Shows the overlay while the process in operation
*/
function showOverlay(message) {
    document.getElementById("overlay").style.display = "block";
    // Add a message to the overlay
    document.getElementById("overlayMessage").append(new Text(message));
}

/*
  Hides the overlay again
*/
function hideOverlay() {
    document.getElementById("overlay").style.display = "none";
    // Remove message
    document.getElementById("overlayMessage").firstChild.remove();
}

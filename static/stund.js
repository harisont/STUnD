/*
  Adds the error class and red background to one element
*/
function markError(element) {
    element.classList.add("error");
    element.style.background="red";
}

/*
  Removes the error class and red background from one element
*/
function resetError(element) {
    element.classList.remove("error");
    element.style.removeProperty("background-color");
}

/*
  Adds a new error message in red to the error div
*/
function addErrorMessage(message) {
    var errordiv = document.getElementById("errorDiv");
    var errorp = document.createElement("p");
    errorp.style.color="red";
    errorp.append(message);
    errordiv.append(errorp);
}

/*
  Remove all error messages from the error div
 */
function removeErrorMessages() {
    var errordiv = document.getElementById("errorDiv");
    while (errordiv.firstChild) {
	// The list is LIVE so it will re-index each call
	errordiv.removeChild(errordiv.firstChild);
    }
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

async function sendData() {
    // Remove all previous errors
    removeErrorMessages();
    resetAllErrors();
    var error = false;
    var l1treebank = document.getElementById("l1treebank").value;
    // The L1 treebank is actually optional
    // if (l1treebank == "") {
    // 	markError(document.getElementById("l1span"));
    // 	addErrorMessage("L1 treebank is required");
    // }
    var l2treebank = document.getElementById("l2treebank").value;
    if (l2treebank == "") {
	markError(document.getElementById("l2span"));
	addErrorMessage("L2 treebank is required");
	error = true;
    }
    var queryElement = document.getElementById("query");
    // Replace empty query by the default value TRUE
    if (queryElement.value == "") {
	queryElement.value="TRUE";
	// queryElement.value="[(DEPREL_ \"root\",DEPREL_ \"root\")]";
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
  });
    }
}

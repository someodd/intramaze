/**
 * Interact with the PinkSands REST API.
 * 
 * Generic PinkSands REST API JavaScript interface.
 */


/**
 * The host used for all REST API requests.
 * @type {String}
 */
 const restApiHost = "http://localhost:3000/";


// FIXME: weirdly on the serverside if you request ..//room/whatever you'll get some kind of response? makes bugs confusing.
/**
 * Make a request to the REST API.
 * @param {String} endpoint - The endpoint to make a request to. See also `restApiHost`.
 * @param {String} method - The HTTP method to use during the request.
 * @param {Object} object - The object to use in the JSON body during the request.
 * @returns {Object} The JSON response.
 */
async function restApiRequest(endpoint, method, object) {
    // Generic request to the REST API.
    const response = await fetch(restApiHost + endpoint, {
      method: method,
      body: (typeof object !== 'undefined') ? JSON.stringify(object) : null
    });
    const responseJSON = await response.json();
    return responseJSON;
  }


/**
 * Update a room's description.
 * @param {String} uuid - ID of the room to update the description of.
 */
async function updateDescription(uuid) {
    // update description.
    var description = document.getElementById('description').value;
    const response = await restApiRequest('rooms/' + uuid, 'PATCH', {description: description})
    console.log(response);
    await regenerateRoom(uuid);
}


/**
 * Update a room's title.
 * @param {String} uuid - ID of the room to update the title of.
 */
 async function updateTitle(uuid) {
    // update title.
    var title = document.getElementById('title').value;
    const response = await restApiRequest('rooms/' + uuid, 'PATCH', {title: title})
    console.log(response);
    await regenerateRoom(uuid);
}
  
  
// FIXME: redirect after deleting.
/**
 * Delete a room.
 * @param {String} uuid - Id of the room to delete.
 */
async function deleteRoom(uuid) {
    // Delete room matching uuid using the REST API.
    const response = await restApiRequest('rooms/' + uuid, 'DELETE')
    console.log(response);
}


/**
 * (Re)create the static files for a specific room.
 * @param {*} uuid - The ID of the room to generate the static files for.
 */
async function regenerateRoom(uuid) {
    // Call upon the REST API endpoint which (re)creates the static files for the room matching the supplied UUID.
    const response = await restApiRequest(`rooms/${uuid}/generate`, 'GET');
    console.log(response);
}


/**
 * Recreate/rebuild all of the site's static files.
 */
async function regenerateEverything() {
    // Calls the REST API endpoint for (re)creating all the static files.
    const response = await restApiRequest('generate', 'GET');
    console.log(response);
}


/**
 * Get a room.
 * 
 * @param {String} uuid - Identifier for the room to fetch (room ID/UUID).
 * @returns {Object} - Room JSON.
 */
async function getRoom (uuid) {
    const response = await restApiRequest('rooms/' + uuid, 'GET');
    console.log(response);
    return response;
}


/**
 * Authentication request (JSON Web Token).
 * 
 * @param {String} username -  
 * @param {String} password -  
 * @returns On success returns the JSON web token. Otherwise an error message.
 */
async function authenticate(username, password) {
    /** post a user login to /users/login, parse response */
    const response = await restApiRequest('users/login', 'POST', {username: username, password: password})
    console.log(response);
    return response;
}


/**
 * Register/create a user account.
 * @param {String} username -
 * @param {String} password -
 * @returns 
 */
async function register(username, password) {
    const response = await restApiRequest('users', 'POST', {username: username, password: password});
    console.log(response);
    return response;
}


/**
 * Perform a JWT-authenticated request to the /user/whoami endpoint.
 * 
 * @param {String} jwt - JSON Web Token...
 * @returns ...
 */
async function whoami(jwt) {
    const response = await restApiRequest('users/whoami', 'GET', {tokenA: jwt});
    console.log(response);
    return response;
}


/**
 * Not necessarily a REST API thing, but it's here because it's shared... I should really move this somewhere else!
 * 
 * @param {String} val - Value to search for.
 * @param {String} buttonId - Button #ID to disable or enable.
 */
async function showResults(val, buttonId) {
    if (val == '' || val.length < 3) {
        document.getElementById(buttonId).disabled = true;
        res = document.getElementById("result");
        res.innerHTML = '';
        let list = '';
        return null;
    }
    // Provide room suggestions based on searching for "val" in room descriptions. Put the suggestions in an element with the ID "result."
    res = document.getElementById("result");
    res.innerHTML = '';
    let list = '';
    //var terms = await autocompleteMatch(val);
    // FIXME: need to handle qstring properly (url sanitize val)
    var terms = val == '' ? [] : await restApiRequest('rooms/search?description=' + val, 'GET');
    for (term in terms) {
        var room = terms[term];
        // this feels very sloppy FIXME. also hardcoding room URL for image is bad
        list += '<div class="result" style="background-image: url(/rooms/' + room.id + '/' + room.bgFileName + ');" onclick="document.getElementById(\'q\').value = \'' + room.id + '\';res.innerHTML=\'\'; document.getElementById(\'' + buttonId + '\').disabled = false;"><dt>' + room.title + ' <span class="small">(' + room.id + ")</small></dt><dd><img src=\'/rooms/" + room.id + "/" + room.bgFileName + "\' width=75px height=75px/>" + room.description + '</dd></div>';
    }
    res.innerHTML = '<dl>' + list + '</dl>';

    // horrible. 
    document.getElementById(buttonId).disabled = true;
    // even simpler, can just check if text is valid uuid for a room...
    if (val.length == 36 ) {
        document.getElementById(buttonId).disabled = await getRoom(val) == null;
    }
}
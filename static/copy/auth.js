/*
Web browser user-side authentication
Handles authenticating to the REST API and maintaining the JSON Web Token.

Huge security risks need to be mitigated:
https://dev.to/gkoniaris/how-to-securely-store-jwt-tokens-51cf
*/


/**
 * Swap content inside of an element if logged in or not, depending on if not null.
 * 
 * @param {*} element - The element whose contents to change.
 * @param {*} ifAuthenticated - If not `null` then if the user is authenticated,
 *  change the insides to this string.
 * @param {*} ifAnonymous - If not `null`, then if the user is NOT logged in,
 *  change the insides to this string.
 */
async function swapInsideAuthenticated(element, ifAuthenticated, ifAnonymous) {
    const response = getUser()
    const isLoggedIn = await response !== null;
    if (ifAuthenticated !== null && isLoggedIn) {
        element.innerHTML = ifAuthenticated
    } else if (ifAnonymous !== null && !isLoggedIn) {
        element.innerHTML = ifAnonymous
    }
}


/**
 * Attempt a login. If successful, store the JSON Web Token in a cookie.
 * 
 * @param {String} username -
 * @param {String} password - 
 */
async function login(username, password) {
    const hopefullyToken = authenticate(username, password);
    console.log(await hopefullyToken)
    console.log("ok")
    /* now set the cookie with the token */
    document.cookie = "token=" + await hopefullyToken;
    const user = await getUser()
    window.location.replace("/users/" + user.userName + ".html")
}

function showErrorMessage(wherein, errorMessage) {
    const errorBox = document.getElementById("error");
    if (errorBox !== null) {
        errorBox.innerHTML = errorMessage;
    } else {
        const newErrorBox = document.createElement("div")
        newErrorBox.className = "error"
        newErrorBox.innerHTML = errorMessage
        wherein.appendChild(newErrorBox)
    }
}

/**
 * Register a user, log them in, and redirect to profile.
 * 
 * If a user is already logged in, fail.
 * 
 * @param {String} username 
 * @param {String} password 
 */
async function registerRedirect(username, password) {
    const response = await register(username, password);

    if (response.hasOwnProperty("errorStatus")) {
        showErrorMessage(document.getElementById("login-form"), response.errorName + response.errorMessage)
        console.log("matched name")
    } else if (response.successStatus) {
        login(username, password);
    } else {
        console.log("should be impossible. broken api.")
    }
    
}

async function testAuth() {
    console.log(await whoami(getJwtCookie()));
}


/**
 * Can be used to test if user is logged in too.
 * @returns Object|Null
 */
async function getUser() {
    const jwtCookie = getJwtCookie();
    if (jwtCookie) {
        const response = await whoami(jwtCookie);
        return typeof response.error === "undefined" ? response : null
    } else {
        return null
    }
}


/**
 * Get the JSON Web Token from cookie.
 * @returns {String|null} - Null if no "token" found.
 */
function getJwtCookie() {
    const cookiePairs = document.cookie.split(';');
    for (const cookiePair of cookiePairs) {
        const [key, ...value] = cookiePair.split('=');
        console.log(key.trim())
        if (key.trim() === "token") {
            console.log("found token");
            return value;
        }
    }
    console.log("never found cookie")
    return null;
}

/**
 * Simply delete the JWT cookie.
 */
function logout() {
    
}
/*
Web browser user-side authentication
Handles authenticating to the REST API and maintaining the JSON Web Token.

Huge security risks need to be mitigated:
https://dev.to/gkoniaris/how-to-securely-store-jwt-tokens-51cf
*/


/**
 * Attempt a login. If successful, store the JSON Web Token in a cookie.
 * 
 * @param {String} username -
 * @param {String} password - 
 */
async function login(username, password) {
    const hopefullyToken = authenticate(username, password);
    /* now set the cookie with the token */
    document.cookie = "token=" + await hopefullyToken;
    const user = await getUser()
    window.location.replace("/users/" + user.userName + ".html")
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
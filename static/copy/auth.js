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
}

async function testAuth() {
    document.write(await whoami(getJwtCookie()));
}

/**
 * Get the JSON Web Token from cookie.
 * @returns {String|null} - Null if no "token" found.
 */
function getJwtCookie() {
    const cookiePairs = document.cookie.split(';');
    for (const cookiePair of cookiePairs) {
        const [key, ...value] = cookiePair.split('=');
        if (key === "token") {
            return value;
        }
    }
    return null;
}
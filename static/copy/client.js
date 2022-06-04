/* web chat client */
/* maybe i should represent state of people and what they're saying and run that
through the drawing function. */

/* These globals are for drawing on the image for the chat. */
var roomImage = document.getElementById('roomChatCanvas');


/***
 * A representation of the users and the text they're saying.
 * 
 * Used for drawing. Useful for wiping and then redrawing each
 * update from this data model.
 * 
 * The name is simply the username. The "message" is the last
 * message they sent, which will take the form of a speech
 * bubble/balloon.
 * 
 * @type {String[][]} - in the form of [[name, message]].
 */
var representation = []


/**
 * Update the representation with a new message from a user.
 * 
 * @param {String} usernameToFind - User in question.
 * @param {String} message - The most current message the user sent.
 * @returns {Boolean} - True if found and updated user. False if
 *   had to create a new entry.
 */
 function updateRepresentationMessage(usernameToFind, newMessage) {
    for (index in representation) {
        var [username, oldMessage] = representation[index]
        
        if (usernameToFind == username) {
            representation[index] = [username, newMessage]
            return true
        }
    }
    // nothing was found so let's make the record!
    representation.push([usernameToFind, message])
    return false
}


function addUserRepresentation(userToAdd) {
    representation.push([userToAdd, null])
    return null
}


function removeUserRepresentation(userToRemove) {
    for (index in representation) {
        var [username, message] = representation[index]
        if (username == userToRemove) {
            representation.splice(index, 1)
            return true
        }
    }
    return false
}

function resetCanvas() {
    document.getElementById("roomChatCanvas").innerHTML = ''
}

/**
 * Wipe and then render anew the chat.
 * 
 * @param {String[][]} representation - see the "representation"
 *   global variable.
 */
function drawRepresentation () {
    // clear the "canvas" before we re/draw the representation
    resetCanvas()
    for (index in representation) {
        var [username, message] = representation[index]
        drawUser(username, index)
        if (message) drawMessageBalloon(username, index, message)
    }
}


/**
 * Draw a user on the room. Used in drawing the representation.
 * 
 * @param {String} username - ...
 * @param {Number} nth - This is the nth user?
 */
 function drawUser(username, n) {
    const roomRect = roomImage.getBoundingClientRect();

    var node = document.createElement("div");

    var identicon = document.createElement('identicon-svg');
    identicon.width = userAvatarWidth;
    identicon.height = userAvatarHeight;
    identicon.setAttribute('username',  username);
    identicon.title = username;
    node.appendChild(identicon);

    var label = document.createElement('span');
    label.style.color = stringToColor(username);
    label.style.fontWeight = "bold";
    label.style.fontSize = "0.75rem";
    label.style.display = "block";
    var text = document.createTextNode(username);
    label.appendChild(text);
    node.appendChild(label);

    document.getElementById('roomChatCanvas').appendChild(node);

    [leftPosition, topPosition] = userAvatarPosition(n);

    // finally position the node which contains both the identicon
    // and the label/username
    node.style.position = "absolute";
    node.style.left = leftPosition + "px";
    node.style.top = topPosition + "px";
    node.style.zIndex = "99";
}

/**
 * Hash a string to a hex code color.
 * 
 * Related to drawing the representation.
 * 
 * Credit: https://stackoverflow.com/questions/3426404/create-a-hexadecimal-colour-based-on-a-string-with-javascript
 * 
 * @param {String} str - The seed/thing to be hashed into a hex code color.
 * @returns {String} - Hex code color.
 */
var stringToColor = function(str) {
    var hash = 0;
    for (var i = 0; i < str.length; i++) {
        hash = str.charCodeAt(i) + ((hash << 5) - hash);
    }
    var colour = '#';
    for (var i = 0; i < 3; i++) {
        var value = (hash >> (i * 8)) & 0xFF;
        colour += ('00' + value.toString(16)).substr(-2);
    }
    return colour;
}

/**
 * Create/overlay the chat canvas onto the room image/map.
 * 
 * This is where the representation gets drawn.
 */
function overlayCanvas()
{
    var img = document.getElementById('dynamic_img'); 
    var width = img.width;
    var height = img.height;

    //img.style.position = "absolute";
    img.style.zIndex = "1";

    // FIXME: change roomChatCanvas into a div?
    var canvas = document.getElementById('roomChatCanvas'); 
    canvas.style.width = width + "px";
    canvas.style.left = "0px"
    canvas.height = height;
    canvas.style.zIndex = 20;
    canvas.style.position = "block";
    canvas.style.margin = "1.5rem 20px";
    canvas.style.backgroundColor = "#eee";
}


const userAvatarHeight = 80;
const userAvatarWidth = 80;
const spaceBetweenAvatars = userAvatarHeight * 2;  // vertical
const topPadding = 100; // give avatars room from top of canvas so we have room to draw speech balloons.

/**
 * The top left position of the user avatar based on which
 * number user this is (n).
 * 
 * Is handy also for knowing where to draw balloons. This is
 * used in drawing the representation.
 * 
 * @param {Number} n - The nth user in the user list.
 * @returns {Number []} - (x, y) position as left, top pixel
 *  coordinates.
 */
function userAvatarPosition(n) {
    const roomRect = roomImage.getBoundingClientRect();
    // now for some math to determine the position of the player.
    // even goes on left odd goes on right. spacing down each row.
    const leftPosition = n % 2 == 0 ? roomRect.left : roomRect.right - userAvatarWidth;
    // how to invert below?
    //const topPosition = topPadding + roomRect.top + (Math.floor(n / 2) * spaceBetweenAvatars);
    const topPosition = ((userAvatarHeight * -1 ) + 0) - (Math.floor(n / 2) * spaceBetweenAvatars);
    return [leftPosition, topPosition]
}


/**
 * Remove a user from the room.
 * @param {String} username 
 */
function removeUser(username) {

}

/* old stuff below*/

function createChatSocket() {
    return new WebSocket('ws://localhost:3000/');
}

/**
 * Render a message from #n user by username on the canvas in the
 * form of a cartoon speech balloon.
 * @param {*} username - The user who sent the message.
 * @param {*} n - The position the user occupies in the list
 *   of users.
 * @param {*} message - The speech.
 */
function drawMessageBalloon(username, n, message) {
    const roomRect = roomImage.getBoundingClientRect();
    const balloonId = username + n;

    var text = document.createTextNode(message);

    if (document.getElementById(balloonId)) {
        document.getElementById(balloonId).replaceChildren(text)
        // below is hacky, repeated code
        var tri = document.createElement("div")   
        tri.style.position = "absolute"
        tri.style.zIndex = "9999999"
        tri.style.left = "50px"
        tri.style.bottom = "-40px"
        //tri.style.top = "1.5rem"
        tri.style = "position: absolute;width: 0;height: 0;left: 30px;right: auto;top: auto;bottom: -40px;border: 20px solid;    border-top-color: currentcolor;    border-right-color: currentcolor;    border-bottom-color: currentcolor;    border-left-color: currentcolor;border-color: #666 transparent transparent #666;"
        document.getElementById(balloonId).appendChild(tri)
        // left line of triangle
        var tri2 = document.createElement("div")
       tri2.style = "position: absolute;width: 0;height: 0;left: 38px;right: auto;top: auto;bottom: -20px;border: 12px solid;    border-top-color: currentcolor;    border-right-color: currentcolor;    border-bottom-color: currentcolor;    border-left-color: currentcolor;border-color: rgba(248, 246, 125) transparent transparent rgba(248, 246, 125);"
        document.getElementById(balloonId).appendChild(tri2)
    } else {
        var node = document.createElement("div"); // speech bubble
        node.id = username + n;

        var textNode = document.createElement("div");
        textNode.appendChild(text);
        textNode.style.overflowY = "auto";
        textNode.style.overflowX = "auto";
        textNode.style.maxHeight = "3rem";
        node.appendChild(textNode);

        document.getElementById('roomChatCanvas').appendChild(node);

        [leftPosition, topPosition] = userAvatarPosition(n);

        // finally position the node which contains both the identicon
        // and the label/username
        var speechBalloonLeftOffset = n % 2 == 0 ? userAvatarWidth : (userAvatarWidth * -1)
        var speechBalloonTopOffset = (userAvatarHeight + 35) * -1;
        node.className = "speechBalloon"; // doesn't work i guess
        node.style.backgroundColor = "rgba(248, 246, 125)";
        node.style.padding = "1.5rem 20px"
        node.style.border = "12px solid #666";
        node.style.maxHeight = "6rem";
        node.style.maxWidth = "40%";
        node.style.borderRadius = "10px";
        node.style.position = "absolute";
        node.style.left = (leftPosition + speechBalloonLeftOffset) + "px";  // but if it's left then add left padding else right padding?
        node.style.top = (topPosition + speechBalloonTopOffset) + "px";
        node.style.zIndex = "99";
        // a little logic for drawing the little speech triangle
        // the speech triangl eain't working
        /*
content: "";
position: absolute;
z-index: 10;
bottom: -40px;
left: 50px;
width: 50px;
height: 30px;
border-style: solid;
border-width: 0 10px 10px 0;
border-color: #5a8f00;
background: transparent;
-webkit-border-bottom-right-radius: 80px 50px;
-moz-border-radius-bottomright: 80px 50px;
border-bottom-right-radius: 80px 50px;
display: block;
        */
        // right line of triangle
        var tri = document.createElement("div")   
        tri.style.position = "absolute"
        tri.style.zIndex = "9999999"
        tri.style.left = "50px"
        tri.style.bottom = "-40px"
        //tri.style.top = "1.5rem"
        tri.style = "position: absolute;width: 0;height: 0;left: 30px;right: auto;top: auto;bottom: -40px;border: 20px solid;    border-top-color: currentcolor;    border-right-color: currentcolor;    border-bottom-color: currentcolor;    border-left-color: currentcolor;border-color: #666 transparent transparent #666;"
        node.appendChild(tri)
        // left line of triangle
        var tri2 = document.createElement("div")
       tri2.style = "position: absolute;width: 0;height: 0;left: 38px;right: auto;top: auto;bottom: -20px;border: 12px solid;    border-top-color: currentcolor;    border-right-color: currentcolor;    border-bottom-color: currentcolor;    border-left-color: currentcolor;border-color: rgba(248, 246, 125) transparent transparent rgba(248, 246, 125);"
        node.appendChild(tri2)
    }

}

/**
 * Find the index/number the username is within the representation.
 * 
 * @param {String} usernameToFind - The username to get the index
 *   of in the representation.
 * @returns - the index if found, or null if not found.
 */
function userIndex(usernameToFind) {
    for (index in representation) {
        var [username, message] = representation[index]
        if (usernameToFind == username) {
            return index
        }
    }
    return null
}

// this is hacky
function usernameNumberMessage(message) {
    var [username, ...rest] = message.split(":")
    var number = userIndex(username)
    return [username, number, rest]
}

function onMessage(event) {
    var p = $(document.createElement('p')).text(event.data);
    var [username, number, restOfMessage] = usernameNumberMessage(event.data);
    
    $('#messages').append(p);
    $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});

    if(event.data.match(/^[^:]* joined/)) {
        var user = event.data.replace(/ .*/, '');
        addUserRepresentation(user)
    } else if(event.data.match(/^[^:]* disconnected/)) {
        var user = event.data.replace(/ .*/, '');
        removeUserRepresentation(user)
    } else {
        updateRepresentationMessage(username, restOfMessage)
    }
    drawRepresentation()
}

/**
 * Set up th erepresentation global with a bunch of usernames.
 * 
 * @param {String[]} users - The list of usernames already
 *   in the room.
 */
function initializeRepresentation(users) {
    for (index in users) {
        representation.push([users[index], null])
    }
}


$(document).ready(function () {
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var user = $('#user').val();
        var uuid = $('#rowUuid').val();
        var ws = createChatSocket();


        ws.onopen = function() {
            ws.send(`Hello, ${uuid}! I am ${user}`);
        };

        ws.onmessage = function(event) {
            if(event.data.match('^Welcome! Users: ')) {
                /* Calculate the list of initial users */
                var str = event.data.replace(/^Welcome! Users: /, '');
                if(str != "") {
                    // does this overwrite the global? scoping weird.
                    // get rid of user global soon.
                    users = str.split(", ");
                    initializeRepresentation(users)
                    // FIXME: what's the poin tof this block, anyway?
                    //refreshUsers();
                }

                $('#join-section').hide();
                $('#chat-section').show();
                $('#users-section').show();

                ws.onmessage = onMessage;

                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send(text);
                    $('#text').val('');
                    return false;
                });
            } else {
                $('#warnings').append(event.data);
                ws.close();
            }
        };

        $('#join').append('Connecting...');

        return false;
    });
});

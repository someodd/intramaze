/**
 * Edit a room using the portal editor interface.
 */


/**
 * The polygon canvas is where all the portal drawing/creation and deletion happens.
 */
var polygonCanvas = document.getElementById('polygonCanvas');
var ctx = polygonCanvas.getContext('2d');
ctx.fillStyle = 'rgb(255,165,0,0.5)';
ctx.strokeStyle = 'rgb(165,255,0,0.75)';

/**
 * Global for storing points collected when a user is drawing a polygon for a new portal.
 * @type {Number[][]}
 */
var points = [];
// FIXME: rename above?


/**
 * Keeps track of whether portal drawing was started or not, indicating if the current mode is polygon drawing mode.
 */
var isDrawingPolygon = false;


/**
 * Using the points collected using the
 * portal polygon drawing feature of the room editor canvas
 * @param {String} someUuid - Identifier for the room being linked to with this portal.
 */
async function makePortal(someUuid) {
  var linksTo = document.getElementById('q').value;
  const object =  {
    linksTo: linksTo,
    belongsTo: someUuid,
    coordinates: points
  };
  console.log(object);
  const responseText = await restApiRequest('portals', 'POST', object);
  console.log(responseText);
  await afterUpdate(someUuid);
}

/**
 * Regenerate the room and reload the page.
 * 
 * This is useful for after updating a piece of the room's information.
 * 
 * @param {String} someUuid - The room to regenerate. 
 * @returns 
 */
async function afterUpdate(someUuid) {
  await regenerateRoom(someUuid);
  location.reload();
}


// FIXME: why is this setting fill and stroke style?
function getCursorPosition(event) {
  const canvasRect = polygonCanvas.getBoundingClientRect();
  ctx.fillStyle = 'rgb(255,165,0,0.5)'
  ctx.strokeStyle = 'rgb(165,255,0,0.75)'
  const x = event.clientX - canvasRect.left;
  const y = event.clientY - canvasRect.top;
  return [x, y];
}


/**
 * Convert a series of (x, y) coordinates to a series of offsets, each a difference
 * in pixels from the preceeding item.
 * 
 * This is useful because of how drawing lines works in JavaScript with the lineTo()
 * function. You draw lines by calling lineTo() with (x,y) offsets, which directs
 * where the line is being drawn to relative to its current position.
 * 
 * @param {Number[][]} points Coordinates in the form of [[x, y]] representing
 *  positional coordinates which are being used in the process of making a polygon.
 * @returns {Number[][]} The coordinates as deltas.
 */
function pointsAsDeltas (points) {
  console.log(typeof(points));
  var pointDeltas = []
  for (pointIndex in points) {
    if (pointIndex == 0) {
      pointDeltas.push([0,0])
    } else {
      var lastPoint = points[pointIndex - 1]
      var lastPointX = lastPoint[0]
      var lastPointY = lastPoint[1]
      
      var currentPoint = points[pointIndex]
      var currentX = currentPoint[0]
      var currentY = currentPoint[1]

      pointDeltas.push([lastPointX - currentX, lastPointY - currentY])
    }
  }
  return pointDeltas
}


// FIXME: should instead use a more gamedev type rendering loop process
function clearCanvas () {
  ctx.clearRect(0, 0, polygonCanvas.width, polygonCanvas.height);
}


/**
 * Draw the polygon made up from a series of points onto the canvas.
 * @param {Number[][]} points 
 */
function drawPolygon(points) {
  // this shouldn't happen here! it's interesting though because it could be used to change the style of different operations. should be done on global level, though
  ctx.fillStyle = 'rgb(255,165,0,0.5)'
  ctx.strokeStyle = 'rgb(165,255,0,0.75)'

  // continue...
  ctx.beginPath();
  var firstPoint = points[0]
  ctx.moveTo(firstPoint[0], firstPoint[1]);
  for ( point in pointsAsDeltas(points) ) {
      var x = points[point][0];
      var y = points[point][1];
      ctx.lineTo(x, y);
  };
  if (points.length > 2) {
    ctx.fill();
    ctx.stroke()
  } else {
    ctx.stroke()
  }
  ctx.closePath()
}


/**
 * Draw the pending polygon using the confirmed points already a part
 * of the polygon and finally the current pending x and y coordinates.
 * @param {*} points - The plots belonging to the polygon being drawn
 *  by the user.
 * @param {*} pendingX - The current pending x coordinate to add to
 *  the polygon being drawn by the user.
 * @param {*} pendingY - Same as pendingX but for the y coordinate.
 */
function drawPending(points, pendingX, pendingY) {
    clearCanvas()
    var pointsCopy = [...points]
    pointsCopy.push([pendingX, pendingY])
    drawPolygon(pointsCopy)
}


polygonCanvas.addEventListener('dblclick', function(mouseEvent){ 
  // do something on double click (finish the polygon)
  if (isDrawingPolygon) {
    drawPolygon(points)
    console.log( points );
    console.log("ended");
    console.log();
    isDrawingPolygon = false;
    document.getElementById('selectRoomLink').style.display = "block";

  } else {
  disableSave();
  const firstPoint = getCursorPosition(mouseEvent);
  points = []
  points.push ( firstPoint );
  console.log("started");
  isDrawingPolygon = true;
 };
});


polygonCanvas.addEventListener('click', function (mouseEvent) {
    if (isDrawingPolygon) {
      appendPosition(mouseEvent);
      drawPolygon(points)
  };
});


// FIXME
// should use points arg? or no?
/**
 * Take the mouse's position and translate it to the relative position
 * on the canvas, and add that to the global for the pending polygon
 * being drawn by the user.
 * @param {Object} mouseEvent - information pertaining to a position
 *  of the cursor.
 */
function appendPosition (mouseEvent) {
  var point = getCursorPosition(mouseEvent)
  var [x, y] = point
  var [oldX, oldY] = points.slice(-1)[0]
  // Don't add a point if it's exactly the same point as the last point added.
  // This also makes double-clicking not append the same coordinate on the
  // second click, registering the double click.
  if ( ! (x == oldX && y == oldY) )
    points.push( point );
};


polygonCanvas.addEventListener('mousemove', function (mouseEvent) {
  if (isDrawingPolygon) {
    var [currentX, currentY] = getCursorPosition(mouseEvent);
    drawPending(points, currentX, currentY)
  }
});


/* Canvas editor: view and remove portals.
 */

var isViewDeleteMode = false;
// this is inconsistent and probably bad?
var portalsGlobal = [];

/**
 * Draw all the portals belonging to a room.
 * @param {String} uuid - Draw the portals belonging to the room of this ID.
 */
async function drawAllPortals(uuid) {
  // Draw all the portals for a given room (by uuid) onto the canvas.
  isViewDeleteMode = true;
  // first we will get the portals for the uuid (room) using REST
  const portals = await restApiRequest(`rooms/${uuid}/portals`, 'GET');
  portalsGlobal = portals;
  clearCanvas();
  for ( portal in portals ) {
    const polygon = portals[portal].coordinates;
    drawPolygon(polygon);
  };
}

/**
 * Detect the first (if any) portal the supplied x and y coordinates are inside of.
 * @param {Object[]} portals - The portal objects (with a "coordinate" attribute
 *  representing the portal's polygon).
 * @param {*} x - Coordinate.
 * @param {*} y - Coordinate.
 * @returns {Object} - The first portal which has a polygon ("coordinate" attribute)
 *  which the (x,y) coordinate is inside of.
 */
function detectWhichPortalInside(portals, x, y) {
  for (portalIndex in portals) {
    const portal = portals[portalIndex];
    if ( inside([x,y], portal.coordinates) ) {
      return portal;
    }
  }
}

// could write my own from: https://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
// stolen from https://stackoverflow.com/questions/22521982/check-if-point-is-inside-a-polygon
function inside(point, vs) {
  // ray-casting algorithm based on
  // https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html/pnpoly.html
  
  var x = point[0], y = point[1];
  
  var inside = false;
  for (var i = 0, j = vs.length - 1; i < vs.length; j = i++) {
      var xi = vs[i][0], yi = vs[i][1];
      var xj = vs[j][0], yj = vs[j][1];
      
      var intersect = ((yi > y) != (yj > y))
          && (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
      if (intersect) inside = !inside;
  }
  
  return inside;
};


// right click will delete portal...
polygonCanvas.addEventListener('contextmenu', async function (mouseEvent) {
  mouseEvent.preventDefault()
  if (isViewDeleteMode) {
    var [currentX, currentY] = getCursorPosition(mouseEvent);
    var portalInside = detectWhichPortalInside(portalsGlobal, currentX, currentY);
    if (portalInside) {
      const responseJSON = await restApiRequest('portals/' + portalInside.id, 'DELETE');
      console.log(responseJSON);
      await afterUpdate(portalInside.belongsTo);
    }
  }
});

// should rename these to *RoomEditor()
function openPortalEditor() {
  document.getElementById('room-view').style.display = "none";
  document.getElementById('admin-room-tools').style.display = "grid";
  document.getElementById('admin-room-tools-opener').style.display = "none";
  document.getElementById('admin-room-tools-closer').style.display = "inline";
  document.getElementById('portal-editor-canvas-area').style.display = "block"; 
  document.getElementById('room-description').style.display = "none"; 
  document.getElementById('page-title').style.display = "none"; 

  resizeCanvas();
}

function closePortalEditor() {
  document.getElementById('room-view').style.display = "block";
  document.getElementById('admin-room-tools').style.display = "none";
  document.getElementById('admin-room-tools-opener').style.display = "inline-block";
  document.getElementById('admin-room-tools-closer').style.display = "none";
  document.getElementById('portal-editor-canvas-area').style.display = "none"; 
  document.getElementById('room-description').style.display = "block"; 
  document.getElementById('page-title').style.display = "block";
}


/**
 * Resize the canvas to the image it contains/to the room image.
 */
function resizeCanvas()
{
    var img = document.getElementById('dynamic_img'); 
    var width = img.width;
    var height = img.height;

    var canvas = document.getElementById('polygonCanvas'); 
    canvas.width = width;
    canvas.height = height;
}

// FIXME/TODO: allowSave()
/**
 * Disable the save portal button and (hide the) link selection.
 */
function disableSave()
{
  document.getElementById('save-portal-button').disabled = true;
  document.getElementById('selectRoomLink').style.display = "none"; 
}
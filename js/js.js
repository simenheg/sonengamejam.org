var notes = [];

function updateNotes(response)
{
    notes = jQuery.parseJSON(response.firstChild.firstChild.nodeValue);
}

function getMousePos(canvas, evt)
{
    var rect = canvas.getBoundingClientRect();
    
    return {
        x: evt.clientX - rect.left,
        y: evt.clientY - rect.top
    };
}

function drawNotes(context)
{
    if (notes == null)
        return;

    for (var i = 0; i < notes.length; i++)
    {
        var note = notes[i];
        var text = note['text'];
        var x = note['x'];
        var y = note['y'];

        // context.beginPath();
        // context.fillStyle = 'yellow';
        // context.rect(x, y - 10, 40, 20);
        // context.fill();

        context.fillStyle = 'black';
        context.fillText(text, x, y);
    }
}

function drawNeedle(context, canvas_width, mouse)
{
    var skew = 0.1 * (mouse.x - canvas_width/2);
    
    // Draw needle
    context.beginPath();
    context.moveTo(mouse.x, mouse.y);
    context.lineTo(mouse.x + skew, mouse.y - 50);
    context.stroke();

    // Draw needle head
    context.fillStyle = 'red';
    context.beginPath();
    context.arc(mouse.x + skew, mouse.y - 50, 10, 0, 2*Math.PI);
    context.fill();
    context.stroke();

    // Draw instruction string
    context.fillStyle = 'black';
    context.fillText("Click to post a note!",
                     mouse.x + 15 + skew/2, mouse.y - 20);
}

function drawBoard(canvas, mouse)
{
    var context = canvas.getContext('2d');

    context.clearRect(0, 0, canvas.width, canvas.height);

    drawNotes(context);
    drawNeedle(context, canvas.width, mouse);
}

function initCanvas()
{
    var canvas = document.getElementById('matchmaking-canvas');

    ajax_get_notes(updateNotes);
    
    canvas.addEventListener('mousemove', function(evt) {
        var mousePos = getMousePos(canvas, evt);
        var message = 'Mouse position: ' + mousePos.x + ',' + mousePos.y;
        drawBoard(canvas, mousePos);
    }, false);

    canvas.addEventListener('mousedown', function(evt) {
        var mousePos = getMousePos(canvas, evt);
        var message = 'mouse pos: (' + mousePos.x + ', ' + mousePos.y + ')';
        ajax_post_note(prompt('Post note:'), mousePos.x, mousePos.y, null);
        ajax_get_notes(updateNotes);
    }, false);
}

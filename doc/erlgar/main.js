var gameArea = {
    canvas : document.createElement("canvas"),
    mousePos : {x:0,y:0},
    init : function() {
        this.canvas.width = 960;
        this.canvas.height = 480;
        this.context = this.canvas.getContext("2d");
        document.body.insertBefore(this.canvas, document.body.childNodes[0]);
    },
    clear : function() {
        gameArea.context.clearRect(0, 0, gameArea.canvas.width, gameArea.canvas.height);
    },
    draw : function(width,height,color,x,y,type,text) {
        ctx = gameArea.canvas.context;
        if (type == "text") {
            ctx.font = width + " " + height;
            ctx.fillStyle = color;
            ctx.fillText(text, x, y);
        } else {
            ctx.fillStyle = color;
            ctx.fillRect(x, y, width, height);
        }
    },
    getMousePos : function() {
        return jrts.jsToErlang([gameArea.mousePos.x,gameArea.mousePos.y]);
    },
    getCanvasSize : function() {
        return jrts.jsToErlang([gameArea.canvas.width,gameArea.canvas.height]);
    },
    debug : true
}

function initGame(){
    gameArea.init();
    erlgar.init();
    setInterval(erlgar.update, 20);
}

function calcMousePos(canvas, evt) {
    var rect = gameArea.canvas.getBoundingClientRect();
    return {
        x: evt.pageX - rect.left,
        y: evt.pageY - rect.top
    };
}

document.addEventListener('touchstart', handleTouchEvent, true);
document.addEventListener('touchmove', handleTouchEvent, true);
document.addEventListener('touchend', handleTouchEvent, true);
document.addEventListener('touchcancel', handleTouchEvent, true);
document.addEventListener('mousemove', handleMouseEvent);

function handleMouseEvent(e) {
    gameArea.mousePos = calcMousePos(gameArea.canvas,e);
    if(gameArea.debug){
        gameArea.clear();
        gameArea.draw(10,10,"black",10,10,"text",""+gameArea.mousePos.x+","+gameArea.mousePos.y);
    }
}

function handleTouchEvent(e) {
    if (e.touches.length === 0 ) return;
    e.preventDefault();
    e.stopPropagation();
    var touch = e.touches[0];
    gameArea.mousePos = calcMousePos(gameArea.canvas,touch);
    if(gameArea.debug){
        gameArea.clear();
        gameArea.draw(10,10,"black",10,10,"text",""+gameArea.mousePos.x+","+gameArea.mousePos.y);
    }
}
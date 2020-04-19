// todo: 
// - the losing click should be highlighted differently
// - flags should be un-right-clickable
// - flags should prevent floodfill over them 
// - when the game ends, incorrect flags will have crossed-out mines
// - when the game ends, correct flags will remain on the board
// - numbers should have the following colorings
// - Number coloring (see picture)
const CANVASWIDTH = 800;
const CANVASHEIGHT = 800;
const BOARDSIZE = 20;
const XSHIFT = 100;
const YSHIFT = 100;
const CELLSIZE = 30;
const TOTALMINES = 80;
let explosionSound;
let stepSound;
let timerValue = 0;
let hiddenCount = 0;
let mouseOnCell = false;
let buttonDown = false;
let firstRevealed = false;
let WS = initialBoard();
function initialBoard() {
    const board = [];
    for (let r = 0; r < BOARDSIZE; r++) {
        board.push([]);
        for (let c = 0; c < BOARDSIZE; c++) {
            let neighbors = getNeighbors(r, c, BOARDSIZE, BOARDSIZE)
            const cell = {
                x: r,
                y: c,
                isHidden: true,
                hasMine: false,
                neighbors: neighbors,
                number: 0,
                flag: false
            };
            board[r][c] = cell;
        }
    }
    return board;
}
function posnToNumber(posn, rows) {
    return posn.x * rows + posn.y;
}

// Posn, Posn[][] -> Number[]
function generateMines(board, firstClick, minesNeeded) {

    let firstClickNum = posnToNumber(firstClick, BOARDSIZE);
    let neighbors = getNeighbors(firstClick.x, firstClick.y, BOARDSIZE, BOARDSIZE)
    let neighborsAsNum = neighbors.map(n => {
        return posnToNumber(n, BOARDSIZE)
    })

    // board as Num + firstClickRemoved
    let numbers = [].concat.apply([], board).map(p => {
        return posnToNumber(p, BOARDSIZE);
    });

    // remove firstClick element
    numbers[firstClickNum] = undefined;

    // remove all the other ones too
    for (i = 0; i < neighborsAsNum.length; i++) {
        numbers[neighborsAsNum[i]] = undefined;
    }

    let removedUndefs = numbers.filter(function (x) {
        return x !== undefined;
    });

    let randomPosns = myShuffle(removedUndefs).map(n => {
        return numberToPosn(n, BOARDSIZE);
    }).slice(0, minesNeeded);

    return randomPosns;
}

function numberToPosn(number, rows) {
    let x = Math.floor(number / rows);
    let y = number - x * rows;
    return { x: x, y: y };
}

function myShuffle(array) {
    var i = array.length,
        j = 0,
        temp;
    while (i--) {
        j = Math.floor(Math.random() * (i + 1));
        temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    return array;
}
function onLeftEdge(x) {
    return x === 0;
}
function onRightEdge(x, xmax) {
    return x === xmax - 1;
}
function onTopEdge(y) {
    return y === 0;
}
function onBottomEdge(y, ymax) {
    return y === ymax - 1;
}
function ismiddle(x, y, ymax, xmax) {
    return x > 0 && x < xmax - 1 && y > 0 && y < ymax - 1;
}
function istopleft(x, y) {
    return onLeftEdge(x) && onTopEdge(y);
}
function istopright(x, y, xmax) {
    return onRightEdge(x, xmax) && onTopEdge(y);
}
function isbottomleft(x, y, ymax) {
    return onLeftEdge(x) && onBottomEdge(y, ymax);
}
function isbottomright(x, y, ymax, xmax) {
    return onRightEdge(x, xmax) && onBottomEdge(y, ymax);
}
function istop(x, y, ymax, xmax) {
    return !istopleft(x, y) && !istopright(x, y, ymax, xmax) && onTopEdge(y);
}
function isbottom(x, y, ymax, xmax) {
    return !isbottomleft(x, y, xmax) && !isbottomright(x, y, ymax, xmax) && onBottomEdge(y, ymax);
}
function isleft(x, y, ymax) {
    return !istopleft(x, y) && !isbottomleft(x, y, ymax) && onLeftEdge(x);
}
function isright(x, y, ymax, xmax) {
    return !isbottomleft(x, y, ymax) && !isbottomright(x, y, ymax, xmax) && onRightEdge(x, xmax);
}
function getBottom(x, y) {
    return { x: x, y: y + 1 };
}
function getTop(x, y) {
    return { x: x, y: y - 1 };
}
function getLeft(x, y) {
    return { x: x - 1, y: y };
}
function getRight(x, y) {
    return { x: x + 1, y: y };
}
function getTopLeft(x, y) {
    return { x: x - 1, y: y - 1 };
}
function getTopRight(x, y) {
    return { x: x + 1, y: y - 1 };
}
function getBottomLeft(x, y) {
    return { x: x - 1, y: y + 1 };
}
function getBottomRight(x, y) {
    return { x: x + 1, y: y + 1 };
}
function getNeighbors(x, y, ymax, xmax) {

    if (ismiddle(x, y, ymax, xmax)) {
        return [
            getTop(x, y),
            getBottom(x, y),
            getLeft(x, y),
            getRight(x, y),
            getTopLeft(x, y),
            getTopRight(x, y),
            getBottomLeft(x, y),
            getBottomRight(x, y)
        ];
    } else if (istopleft(x, y)) {
        return [
            getBottom(x, y),
            getRight(x, y),
            getBottomRight(x, y)
        ];
    } else if (istopright(x, y, xmax)) {
        return [
            getBottom(x, y),
            getLeft(x, y),
            getBottomLeft(x, y)
        ];
    } else if (isbottomleft(x, y, ymax)) {
        return [
            getTop(x, y),
            getRight(x, y),
            getTopRight(x, y)
        ];
    } else if (isbottomright(x, y, ymax, xmax)) {
        return [
            getTop(x, y),
            getLeft(x, y),
            getTopLeft(x, y)
        ];
    } else if (istop(x, y, ymax, xmax)) {
        return [
            getBottom(x, y),
            getLeft(x, y),
            getRight(x, y),
            getBottomLeft(x, y),
            getBottomRight(x, y)
        ];
    } else if (isbottom(x, y, ymax, xmax)) {
        return [
            getTop(x, y),
            getLeft(x, y),
            getRight(x, y),
            getTopLeft(x, y),
            getTopRight(x, y)
        ];
    } else if (isleft(x, y, ymax)) {
        return [
            getTop(x, y),
            getBottom(x, y),
            getRight(x, y),
            getTopRight(x, y),
            getBottomRight(x, y)
        ];
    } else if (isright(x, y, ymax, xmax)) {
        return [
            getTop(x, y),
            getBottom(x, y),
            getLeft(x, y),
            getTopLeft(x, y),
            getBottomLeft(x, y)
        ];
    }
}
function isMouseOnCell(cell) {
    return mouseOnCell && mouseOnCell.x === cell.x && mouseOnCell.y === cell.y;
}
function renderCell(cell) {
    if (cell.isHidden) {
        if (cell.x % 2 === 0 && cell.y % 2 === 0 || cell.x % 2 === 1 && cell.y % 2 === 1) {
            fill("limegreen");
        } else {
            fill("forestgreen");
        }
    } else {
        if (cell.hasMine) {
            fill("crimson");
        } else {
            fill("chocolate");

        }
    }

    if (isMouseOnCell(cell)) {
        fill(150);
        if (buttonDown) {
            fill(220)
        }
    }
    rect(
        cell.x * CELLSIZE + XSHIFT,
        cell.y * CELLSIZE + YSHIFT,
        CELLSIZE,
        CELLSIZE,
    );
    renderNumber(cell);
    renderMine(cell);
    renderFlag(cell);
}
function renderNumber(cell) {
    fill(255);
    if (!cell.isHidden && !cell.hasMine && cell.number !== 0) {
        text(cell.number, cell.x * CELLSIZE + XSHIFT + 9, cell.y * CELLSIZE + YSHIFT + 8, 30, 30);
    }
}
function renderMine(cell) {
    if (!cell.isHidden && cell.hasMine) {
        fill(0);
        ellipse(cell.x * CELLSIZE + XSHIFT + CELLSIZE / 2, cell.y * CELLSIZE + YSHIFT + CELLSIZE / 2, 10)
    }
}
function renderFlag(cell) {
    fill("red");
    if (cell.isHidden && cell.flag) {
        ellipse(cell.x * CELLSIZE + XSHIFT + CELLSIZE / 2, cell.y * CELLSIZE + YSHIFT + CELLSIZE / 2, 10)
    }
}
function renderBoard(board) {
    for (let row = 0; row < BOARDSIZE; row++) {
        for (let col = 0; col < BOARDSIZE; col++) {
            renderCell(board[row][col]);
        }
    }
}
function whereIsMouse() {
    if (
        mouseX > XSHIFT &&
        mouseX < XSHIFT + CELLSIZE * BOARDSIZE &&
        mouseY > YSHIFT &&
        mouseY < YSHIFT + BOARDSIZE * CELLSIZE
    ) {
        mouseOnCell = {};
        mouseOnCell.x = Math.floor((mouseX - XSHIFT) / CELLSIZE);
        mouseOnCell.y = Math.floor((mouseY - YSHIFT) / CELLSIZE);
    } else {
        mouseOnCell = false
    }
}
mousePressed = () => {
    if (mouseOnCell) {
        buttonDown = true;
    } else {
        buttonDown = false;
    }
};
function updateHiddenCount() {
    hidden = 0;
    for (let i = 0; i < BOARDSIZE; i++) {
        for (let j = 0; j < BOARDSIZE; j++) {
            if (WS[i][j].isHidden) {
                hidden++
            }
        }
    }
    hiddenCount = hidden;

}
function gameWon() {
    if (hiddenCount === TOTALMINES) {
        background("forestgreen");
        winning.play();
        setTimeout(() => {
            revealAllMines();
        }, 500);
    }
}
mouseReleased = () => {
    if (mouseButton === LEFT) {
        buttonDown = false;
        revealCell();
        updateHiddenCount();
        gameWon();
    } else if (mouseButton === RIGHT) {
        toggleFlag();
    } else if (mouseButton === CENTER) { }

}
function toggleFlag() {
    if (mouseOnCell) {
        let x = mouseOnCell.x;
        let y = mouseOnCell.y
        let cell = WS[x][y]
        if (cell.isHidden) {
            if (cell.flag === true) {
                cell.flag = false;
            } else if (cell.flag === false) {
                cell.flag = true;
            }
        }
    }
}
function revealAll() {
    for (let i = 0; i < BOARDSIZE; i++) {
        for (let j = 0; j < BOARDSIZE; j++) {
            WS[i][j].isHidden = false;
        }
    }
}
function revealAllMines() {
    for (let i = 0; i < BOARDSIZE; i++) {
        for (let j = 0; j < BOARDSIZE; j++) {
            let cell = WS[i][j];
            if (cell.isHidden && cell.hasMine) {
                cell.isHidden = false;
            }
        }
    }
}
function addMines(board, p, minesNeeded) {
    let mines = generateMines(board, p, minesNeeded);

    mines.forEach(posn => {
        WS[posn.x][posn.y].hasMine = true;
    });
}
function addNumbering() {
    for (let i = 0; i < BOARDSIZE; i++) {
        for (let j = 0; j < BOARDSIZE; j++) {
            let cell = WS[i][j];
            let neighbors = cell.neighbors;
            let number = 0;
            for (let k = 0; k < neighbors.length; k++) {
                if (WS[neighbors[k].x][neighbors[k].y].hasMine) {
                    number++;
                }
            }
            WS[i][j].number = number;
        }
    }
}
function floodfill(x, y) {
    let cell = WS[x][y];
    if (cell.hasMine) {
        // nothing
    } else if (cell.number > 0) {
        cell.isHidden = false;
    } else if (cell.number === 0) {
        cell.isHidden = false;
        let neighbors = cell.neighbors;
        for (let i = 0; i < neighbors.length; i++) {
            let neighborX = neighbors[i].x;
            let neighborY = neighbors[i].y;
            if (WS[neighborX][neighborY].isHidden) {
                floodfill(neighborX, neighborY);
            }
        }
    }
}
function revealCell() {
    if (mouseOnCell) {
        let x = mouseOnCell.x;
        let y = mouseOnCell.y
        let cell = WS[x][y]
        if (cell.isHidden) {
            if (!firstRevealed) {
                firstRevealed = true;
                addMines(WS, { x: x, y: y }, TOTALMINES);
                addNumbering();
            }
            if (cell.hasMine) {
                cell.isHidden = false;
                explosionSound.play();
                setTimeout(() => {
                    revealAllMines();
                }, 500);
            } else {
                cell.isHidden = false;
                stepSound.play();
                floodfill(x, y);
            }
        }
    }
}
function neighborsWithMines(neighborsArray) {
    let totalMines = 0;
    for (let i = 0; i < neighborsArray; i++) {
        if (neighborsArray[i].hasMine) {
            totalMines++;
        }
    }
    return mines;
}
document.oncontextmenu = () => { return false; }
function setup() {
    createCanvas(CANVASWIDTH, CANVASHEIGHT)
    background("saddlebrown");
    textSize(20);
    textFont('Menlo');
    explosionSound = loadSound('explosion.mp3');
    winning = loadSound('tada.mp3');
    stepSound = loadSound('step.mp3');
}
function draw() {
    renderBoard(WS);
    whereIsMouse();
}

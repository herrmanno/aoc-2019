/** @type string[][] */
const map = require("fs").readFileSync("input.txt").toString().split("\n").map(line => line.split(""))
const W = map[0].length
const H = map.length
const LASER = [20, 21]

const angle = ([x1,y1], [x2,y2]) => (Math.atan2(y2 - y1, x2 - x1) * 180 / Math.PI + 270) % 360

const dist = ([x1,y1], [x2,y2]) => Math.sqrt((y2 - y1) ** 2 + (x2 - x1) ** 2)

const asteroids = new Array(H * W).fill(0).map((_, idx) => [(idx % W),~~(idx / W)])
    .filter(([x, y]) => map[y][x] === "#")
    .filter(([x,y]) => !(x === LASER[0] && y === LASER[1]))

const asteroidsWithAngleAndDist = asteroids.map(([x,y]) =>
    [x, y, angle([x,y], LASER), dist([x,y], LASER)]
)

const angles = Array.from(new Set(asteroidsWithAngleAndDist.map(a => a[2]))).sort((a,b) => +a - +b)

let lastAst = null
let ap = 0
for (let i = 0; i < 200; i++, ap = (++ap % angles.length)) {
    const ang = angles[ap]
    const asteroidsWithCurrAngle = asteroidsWithAngleAndDist.filter(a => a[2] === ang)
    lastAst = asteroidsWithCurrAngle.sort((a,b) => a[3] - b[3]).shift()
    asteroidsWithAngleAndDist.splice(asteroidsWithAngleAndDist.indexOf(lastAst), 1)
}


console.log(lastAst)
console.log(lastAst[0] * 100 + lastAst[1])

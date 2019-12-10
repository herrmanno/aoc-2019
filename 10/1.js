/** @type string[][] */
const map = require("fs").readFileSync("input.txt").toString().split("\n").map(line => line.split(""))
const W = map[0].length
const H = map.length

const angle = ([x1,y1], [x2,y2]) => Math.atan2(y2 - y1, x2 - x1)

const asteroids = new Array(H * W).fill(0).map((_, idx) => [(idx % W),~~(idx / W)])
    .filter(([x, y]) => map[y][x] === "#")

const asteroidsWithCount = asteroids.map(([x,y]) => {
    const others = asteroids.reduce((acc, [x2,y2]) => {
        const a = angle([x,y], [x2,y2])
        return { ...acc, [a]: true }
    }, {})

    return [Object.keys(others).length, x, y]
})


const sortedAsteroidsWithCount = asteroidsWithCount.sort(([a], [b]) => a - b).reverse()

console.log(sortedAsteroidsWithCount[0])

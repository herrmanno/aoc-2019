const fs = require("fs")
const i = `R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83`
// const i = fs.readFileSync("input.txt")
const [line1, line2] = i.toString()
    .split("\n")
    .map(line => line.split(",").map(part => part.match(/(\D)(\d+)/).slice(1, 3)))

let minDist = Number.POSITIVE_INFINITY
const dist = arr => arr.map(i => Math.abs(i)).reduce((a, b) => a + b)
const createPath = (commands, path = []) => {
    let [y, x] = [0, 0]
    return commands.reduce((acc, cmd) => {
        for (let i = 0; i < +cmd[1]; i++) {
            if (cmd[0] === "R") {
                [y, x] = [y, x + 1]
            } else if (cmd[0] === "L") {
                [y, x] = [y, x - 1]
            } else if (cmd[0] === "U") {
                [y, x] = [y + 1, x]
            } else if (cmd[0] === "D") {
                [y, x] = [y - 1, x]
            }

            acc[y] = acc[y] || {} 
            acc[y][x] = 1
        
            if (path && path[y] && path[y][x]) {
                // console.dir({y, x, path})
                const d = dist([y, x])
                if (d < minDist && d != 0) {
                    minDist = d
                }
            }
        }

        return acc
    }, {})
}


const path1 = createPath(line1)
createPath(line2, path1)

console.log(minDist)

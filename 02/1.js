const fs = require("fs")
const input = fs.readFileSync("input.txt").toString()

const arr = input.split(",").map(Number)
arr[1] = 12
arr[2] = 2

for (let i = 0; i + 4 <= arr.length; i += 4) {
    const [op, p1, p2, p3] = arr.slice(i, i + 4)
    switch (op) {
        case 1: arr[p3] = arr[p1] + arr[p2]; break;
        case 2: arr[p3] = arr[p1] * arr[p2]; break;
        case 99: break
        default: continue
    }
}

console.log(arr[0])

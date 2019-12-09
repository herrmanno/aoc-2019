const fs = require("fs")
const input = fs.readFileSync("input.txt").toString()
const TARGET_VALUE = 19690720

const arr = input.split(",").map(Number)

function find(arr, noun, verb) {
    arr[1] = noun
    arr[2] = verb
    
    for (let i = 0; i + 4 <= arr.length; i += 4) {
        const [op, p1, p2, p3] = arr.slice(i, i + 4)
        switch (op) {
            case 1: arr[p3] = arr[p1] + arr[p2]; break;
            case 2: arr[p3] = arr[p1] * arr[p2]; break;
            case 99: break
            default: continue
        }
    }

    return arr[0] === TARGET_VALUE
}

for (let i = 0; i <= 99; i++) {
    for (let j = 0; j <= 99; j++) {
        if (find(arr.slice(), i, j)) {
            console.log(100 * i + j)
            process.exit(0)
        }
    }
}

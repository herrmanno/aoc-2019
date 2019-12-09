const fs = require("fs")
const [min, max] = fs.readFileSync("input.txt").toString().split("-").map(Number)

let n = 0

for (let i = min; i <= max; i++) {
    let s = i.toString().split("")
    if (
        s.every((c, idx) => idx === 0 || c >= s[idx - 1]) &&
        s.some((c, idx) => c === s[idx - 1] && c !== s[idx - 2] && c !== s[idx + 1])
    ) {
        n++
    }
}

console.log(n)

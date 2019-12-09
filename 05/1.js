const strs = require("fs").readFileSync("input.txt").toString().split(",")
const INPUT = 1

for (let ip = 0; ip < strs.length;) {
    const [i, a1, a2, a3] = strs.slice(ip)
    const [p1 = "0", p2 = "0"] = i.split("").reverse().slice(2)
    
    if (i == "99") {
        break
    } else if (i.endsWith("4")) {
        console.log(+p1 ? a1 : strs[+a1])
        ip += 2
    } else if (i === "3") {
        strs[+a1] = INPUT
        ip += 2
    } else if (i.endsWith("1")) {
        strs[+a3] = String((+p1 ? +a1 : +strs[a1]) + (+p2 ? +a2 : +strs[a2]))
        ip += 4
    } else if (i.endsWith("2")) {
        strs[+a3] = String ((+p1 ? +a1 : +strs[a1]) * (+p2 ? +a2 : +strs[a2]))
        ip += 4
    } else {
        throw new Error(`Invalid OP Code '${i}'`)
    }
}

const strs = require("fs").readFileSync(__dirname + "/input.txt").toString().split(",")
const INPUT = 2

let relBase = 0
let ip = 0
while (1) {
    const [i = "0", as1 = "0", as2 = "0", as3 = "0"] = strs.slice(ip)
    const instr = +i % 100
    const [p1 = "0", p2 = "0", p3 = "0"] = i.split("").reverse().slice(2)
    const a1 = (+p1 == 1 ? +as1 : +p1 == 2 ? +strs[relBase + +as1] : +strs[+as1]) || 0
    const a2 = (+p2 == 1 ? +as2 : +p2 == 2 ? +strs[relBase + +as2] : +strs[+as2]) || 0
    const a3 = (+p3 == 2 ? relBase + +as3 : +as3) || 0
    
    if (instr == 99) {
        break
    } else if (instr === 1) {
        strs[a3] = String(a1 + a2)
        ip += 4
    } else if (instr === 2) {
        strs[a3] = String(a1 * a2)
        ip += 4
    } else if (instr === 3) {
        strs[relBase + +as1] = String(INPUT)
        ip += 2
    } else if (instr === 4) {
        console.log(a1)
        ip += 2
    } else if (instr === 5) {
        if (a1 != 0) ip = a2; 
        else ip += 3
    } else if (instr === 6) {
        if (a1 == 0) ip = a2; 
        else ip += 3
    } else if (instr === 7) {
        strs[a3] = +(a1 < a2)
        ip += 4
    } else if (instr === 8) {
        strs[a3] = +(a1 == a2)
        ip += 4
    } else if (instr === 9) {
        relBase += a1
        ip += 2
    } else {
        throw new Error(`Invalid OP Code '${i}'`)
    }
}

const strs = require("fs").readFileSync("input.txt").toString().split(",")
const INPUT = 5

for (let ip = 0; ip < strs.length;) {
    const [i, as1, as2, as3] = strs.slice(ip)
    const [p1 = "0", p2 = "0"] = i.split("").reverse().slice(2)
    const a1 = +p1 ? +as1 : +strs[as1]
    const a2 = +p2 ? +as2 : +strs[as2]
    const a3 = +as3
    
    if (i == "99") {
        break
    } else if (i.endsWith("4")) {
        console.log(a1)
        ip += 2
    } else if (i === "3") {
        strs[+as1] = INPUT
        ip += 2
    } else if (i.endsWith("1")) {
        strs[a3] = String(a1 + a2)
        ip += 4
    } else if (i.endsWith("2")) {
        strs[a3] = String(a1 * a2)
        ip += 4
    } else if (i.endsWith("5")) {
        if (a1 != 0) ip = a2; 
        else ip += 3
    } else if (i.endsWith("6")) {
        if (a1 == 0) ip = a2; 
        else ip += 3
    } else if (i.endsWith("7")) {
        strs[a3] = +(a1 < a2)
        ip += 4
    } else if (i.endsWith("8")) {
        strs[a3] = +(a1 == a2)
        ip += 4
    } else {
        throw new Error(`Invalid OP Code '${i}'`)
    }
}

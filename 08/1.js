const input = require("fs").readFileSync("input.txt").toString().split("")

const WIDTH = 25
const HEIGHT = 6
const LAYERS = input.length / (WIDTH * HEIGHT)

const chunks = new Array(LAYERS).fill(0).map((_, idx) => input.slice(idx * WIDTH * HEIGHT, (idx + 1) * WIDTH * HEIGHT))
const min0Layer = chunks.sort((a, b) => {
  const zeros = l => l.filter(c => c === "0").length
  return zeros(a) - zeros(b)
})[0]

const result = min0Layer.filter(c => c === "1").length * min0Layer.filter(c => c === "2").length
console.log(result)
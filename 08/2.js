const input = require("fs").readFileSync("input.txt").toString().split("")

const WIDTH = 25
const HEIGHT = 6
const LAYERS = input.length / (WIDTH * HEIGHT)

const chunks = new Array(LAYERS).fill(0).map((_, idx) => input.slice(idx * WIDTH * HEIGHT, (idx + 1) * WIDTH * HEIGHT))

const getPixel = (chunks, idx) => {
  for (let chunk of chunks) {
    if (chunk[idx] !== "2") return chunk[idx]
  }
}

for (let h = 0; h < HEIGHT; h++) {
  for (let w = 0; w < WIDTH; w++) {
    const pixel = getPixel(chunks, h * WIDTH + w)
    switch (pixel) {
      case "1": process.stdout.write("\u2591"); break;
      default: process.stdout.write(" "); break;
    }
  }
  process.stdout.write("\n")
}
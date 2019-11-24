function initEmpty(width, height) {
  return {
    width,
    height,
    cords: toCords(width, height),
    data: new Set(),
  }
}

function toCords(w, h) {
  const cords = new Array(w * h)
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      cords.push([x, y])
    }
  }
  return cords
}

function randomize(grid) {
  const data = new Set()

  grid.cords.forEach(pos => {
    if (Math.random() < 0.2) {
      data.add(pos)
    }
  })

  return { ...grid, data }
}

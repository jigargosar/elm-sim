const canvasEl = document.getElementById('canvas-el')

const ctx = canvasEl.getContext('2d')

const gridConfig = { rowCount: 30, colCount: 30 }
const canvasSize = { width: ctx.canvas.width, height: ctx.canvas.height }
const cellSize = (canvasSize.width - 2) / gridConfig.colCount
const cellCount = gridConfig.rowCount * gridConfig.colCount

function drawCell(x, y, cell) {
  ctx.beginPath()
  if (cell === 0) {
    ctx.fillStyle = 'yellow'
  } else {
    ctx.fillStyle = 'red'
  }
  ctx.strokeStyle = 'black'
  ctx.lineWidth = 1.5
  ctx.rect(x * cellSize + 1, y * cellSize + 1, cellSize, cellSize)
  ctx.fill()
  ctx.stroke()
}

function randomCell() {
  return Math.random() < 0.2 ? 1 : 0
}

const grid = new Array(cellCount).fill(0).map(randomCell)

grid.forEach((cell, i) => {
  const [x, y] = [
    Math.round(i % gridConfig.colCount),
    Math.floor(i / gridConfig.rowCount),
  ]
  console.log(x, y)
  drawCell(x, y, cell)
})

import { modulo } from 'ramda'

const canvasEl = document.getElementById('canvas-el')

const ctx = canvasEl.getContext('2d')

const gridConfig = { rowCount: 30, colCount: 30 }
const canvasSize = { width: ctx.canvas.width, height: ctx.canvas.height }
const cellSize = (canvasSize.width - 4) / gridConfig.colCount

function drawCell(x, y) {
  ctx.beginPath()
  ctx.fillStyle = 'rgba(0, 0, 200, 0.5)'
  ctx.strokeStyle = 'rgba(0, 0, 0, 0.5)'
  ctx.lineWidth = 1
  ctx.rect(x * cellSize + 1, y * cellSize + 1, cellSize, cellSize)
  ctx.fill()
  ctx.stroke()
}

const cellCount = gridConfig.rowCount * gridConfig.colCount

const grid = new Array(cellCount).fill(0)

grid.forEach((cell, i) => {
  const [x, y] = [
    Math.round(i % gridConfig.colCount),
    Math.floor(i / gridConfig.rowCount),
  ]
  console.log(x, y)
  drawCell(x, y)
})

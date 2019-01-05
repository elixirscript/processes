import '../css/index.css'

import Processes from '../../index.js'

function* render(cell, number) {
  let el = document.getElementById(`td-${cell}`)
  const style = 'group' + (number % 6)
  el.className = style
  el.textContent = number
  yield 0
}

function tableCreate(numberOfRows, numberOfColumns) {
  const main = document.getElementById('main')
  let tbl = document.createElement('table')
  tbl.cellPadding = 0
  tbl.cellSpacing = 0

  let count = 0

  let tbdy = document.createElement('tbody')

  for (let i = 0; i < numberOfRows; i++) {
    let tr = document.createElement('tr')

    for (let j = 0; j < numberOfColumns; j++) {
      let td = document.createElement('td')
      td.id = `td-${count++}`
      td.textContent = 0
      tr.appendChild(td)
    }

    tbdy.appendChild(tr)
  }

  tbl.appendChild(tbdy)
  main.appendChild(tbl)
}

// from https://www.growingwiththeweb.com/2017/12/fast-simple-js-fps-counter.html
const times = []
let fps
let fpsEl

function refreshLoop() {
  window.requestAnimationFrame(() => {
    const now = performance.now()
    while (times.length > 0 && times[0] <= now - 1000) {
      times.shift()
    }
    times.push(now)
    fps = times.length
    if (!fpsEl) {
      fpsEl = document.getElementById('fps')
    }

    fpsEl.textContent = fps.toString()
    refreshLoop()
  })
}

document.addEventListener('DOMContentLoaded', function(event) {
  const system = new Processes.ProcessSystem(
    new Processes.RequestAnimationScheduler()
  )

  const rows = 200
  const columns = 50

  const maxProcesses = rows * columns

  refreshLoop()

  tableCreate(rows, columns)

  for (let i = 0; i < maxProcesses; i++) {
    const _pid = system.spawn(function*() {
      while (true) {
        yield* render(
          Math.floor(Math.random() * maxProcesses),
          Math.floor(Math.random() * 10)
        )
      }
    })
  }
})

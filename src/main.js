import { Elm } from './Main.elm'

const cryptArray = new Uint32Array(4)
const vals = window.crypto.getRandomValues(cryptArray)

const seeds = {
  seed1: vals[0],
  seed2: vals[1],
  seed3: vals[2],
  seed4: vals[3]
}

Elm.Main.init({
  flags: {
    seeds
  },
  node: document.getElementById('app')
})

document.addEventListener('dragstart', (e) => setTimeout(() => {
  e.target.style.opacity = '0'
}, 0))

document.addEventListener('dragend', (e) => {
  e.target.style.opacity = '1'
})

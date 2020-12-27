import { Elm } from './Main.elm'

const seeds = window.crypto.getRandomValues(new Uint32Array(4))

console.log(seeds)

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

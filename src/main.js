import { Elm } from './Main.elm'

const node = document.getElementById('app')

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
  node
})

document.addEventListener('dragstart', (e) => {
  const img = new Image()
  img.src = 'data:image/gif;base64,R0lGODlhAQABAIAAAAUEBAAAACwAAAAAAQABAAACAkQBADs='
  e.dataTransfer.setDragImage(img, 0, 0)
})

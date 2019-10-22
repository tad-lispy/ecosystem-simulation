import * as ElmDebugger from 'elm-debug-transformer';
ElmDebugger.register();

enum Demo { 
  Spreading = "Spreading",
  Binary = "Binary",
  Empty = "Empty"
}

document.addEventListener("DOMContentLoaded", () => {
  const url: URL = new URL(window.location.href)
  const name: string = url.searchParams.get("demo")
  if(!name) {
    return
  }
  const demo: Demo | undefined = Demo[name]
  if(!demo) {
    throw new Error(`Unsupported demo value: ${ name }`)
  }
  load(demo)
})

export default function load(demo: Demo) {
  console.log(demo)
  const name: string = Demo[demo]

  switch(demo) {
    case Demo.Binary:
      return import("./Demos/Binary.elm").then(init(name))
    case Demo.Spreading:
      return import("./Demos/Spreading.elm").then(init(name))
    case Demo.Empty:
      return import("./Demos/Empty.elm").then(init(name))
    default:
      throw new Error(`Unsupported demo value: ${ name }`)
  }
}

function init(name: string) {
  return ({ Elm }) => 
    Elm.Demos[name].init({ 
      node: document.body 
    })
}


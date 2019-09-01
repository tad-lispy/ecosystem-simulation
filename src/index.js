// This module is a wrapper for the Elm app.
// Put initialization logic, ports, etc. here

import { Elm } from "./Main.elm";

const program = Elm.Main;

export function init(node) {
  const main = program.init({
    flags: { },
    node
  });
}

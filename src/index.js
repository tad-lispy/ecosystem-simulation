// This module is a wrapper for the Elm app.
// Put initialization logic, ports, etc. here

import { Elm } from "./Demo.elm";
import * as ElmDebugger from 'elm-debug-transformer';

ElmDebugger.register();

const program = Elm.Demo;

export function init(node) {
  const main = program.init({
    flags: { },
    node
  });
}

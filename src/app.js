// This script is responsible for starting the application.
// Keep is small and dumb. Extend index.js if need be.
// If https://github.com/parcel-bundler/parcel/issues/808 gets resolved then
// we should simply inline it in the HTML file.

import * as app from ".";

app.init(
  document.getElementById(`app-container`)
);

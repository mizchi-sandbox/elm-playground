import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.querySelector("main"),
  flags: Date.now()
});

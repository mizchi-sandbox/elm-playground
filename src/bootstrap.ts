import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.querySelector("main") as HTMLElement,
  flags: Date.now()
});

app.ports.keyPress.subscribe(data => {
  console.log(data);
});
console.log(app);

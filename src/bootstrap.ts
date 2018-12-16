import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.querySelector("main") as HTMLElement
});

app.ports.toJs.subscribe(data => {
  console.log(data);
});
console.log(app);

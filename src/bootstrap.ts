import { Elm } from "./Main.elm";

const _app = Elm.Main.init({
  node: document.querySelector("main") as HTMLElement,
  flags: 6
});

// console.log(app);
// (window as any).app = app;

// app.ports.toJs.subscribe(data => {
//   console.log(data);
// });

type ElmApp = {
  // TODO: wip
  ports: {
    // toJs: {
    //   subscribe(fn: (data: {}) => void): void;
    // };
  };
};

export const Elm: {
  Main: {
    init(opts: { node: HTMLElement; flags: any }): ElmApp;
  };
};

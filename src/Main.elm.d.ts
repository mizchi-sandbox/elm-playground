type ElmApp = {
  ports: {
    keyPress: {
      subscribe(fn: (data: string) => void): void;
    };
  };
};

export const Elm: {
  Main: {
    init(opts: { node: HTMLElement; flags: number }): ElmApp;
  };
};

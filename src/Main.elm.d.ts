type ElmApp = {
  ports: {
    toJs: {
      subscribe(fn: (data: string) => void): void;
    };
  };
};

export const Elm: {
  Main: {
    init(opts: { node: HTMLElement; flags?: any }): ElmApp;
  };
};

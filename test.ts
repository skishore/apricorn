type Foo = {
  a: number,
  b: Bar,
};

type Bar = {
  x: Foo | null,
};

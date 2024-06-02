type Foo = {
  a: number,
  b: Bar,
};

type Bar = {
  x: Foo | null,
};

const f = (x: number): number => {
  return x * x;
};

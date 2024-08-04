type Foo = {
  a: number,
  b: Bar,
};

type Bar = {
  x: Foo | null,
};

const e = (n: number): number => {
  if (n < 2) return 1;
  return f(n - 1) + f(n - 2);
};

const f = (n: number): number => {
  if (n < 2) return 1;
  return e(n - 1) + e(n - 2);
};

const g = (x: number): number => {
  const z = x * x;
  const h = (n: number): number => {
    if (n < 2) return 1;
    return h(n - 1) + h(n - 2);
  };
  const a = h(z);
  return a;
};

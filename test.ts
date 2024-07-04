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

const g = (x: number): number => {
  const z = x * x;
  const h = (): number => z * z;
  const a = h();
  return a;
};

const x = 17;
const z = x * x;
const h = (): number => z * z;
const a = h();

const f = (x: number) => {
  return x > 0 ? g(x - 1) + 1 : 0;
};

const g = (x: number) => {
  return x > 0 ? f(x - 1) + 1 : 0;
};

console.log(f(3));

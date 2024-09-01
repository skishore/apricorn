//{a: (a * x - ~!17++--)(a, [b, 'xyz']), b:, c}
//{a: int, b: number, c: [(T | U)[], string], d: (x: int, y: int) => string}

enum E { A, B };
type A = {tag: E.A, x: X};
type B = {tag: E.B};

type X = A | B;

const x = (x: X | null): void => {};

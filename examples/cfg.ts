const f = (x: number | null): number => {
  let y = -1;

  while (y < 10) {
    if (y < 1) y += 1;
    if (y === 5) break;
    y += 1;
  }

  //if (x === null) {
  //  y = 0;
  //} else if (x <= 1) {
  //  return 1;
  //} else {
  //  y = x * x;
  //}
  return y;
};

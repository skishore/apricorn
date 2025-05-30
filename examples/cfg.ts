const s = (x: number): number => {
  switch (x) {
    case 17:
      x = 23;
    case 23: {
      x = 1;
      break;
    }
    default:
      x = 3;
  }
  return x;
};

//const f = (x: number | null): number => {
//  do {
//    if (!x) x = 17;
//  } while (!x);
//  return x;
//
//  //const y = x === null ? 0 : x;
//  //let result = 0;
//  //for (let i = 0; i < y; i++) {
//  //  result += i * i;
//  //}
//  //return result;
//
//  //let y = -1;
//
//  //while (y < 10) {
//  //  if (y < 1) y += 1;
//  //  if (y === 5) break;
//  //  y += 1;
//  //}
//
//  //if (x === null) {
//  //  y = 0;
//  //} else if (x <= 1) {
//  //  return 1;
//  //} else {
//  //  y = x * x;
//  //}
//  //return y;
//};
//
//const g = (x: number): number => {
//  switch (x) {
//    case 17: case 34: break;
//  }
//  return x * x;
//};

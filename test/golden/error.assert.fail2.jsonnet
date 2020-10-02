local subtract(a, b) =
  assert a > b : 'a must be bigger than b';
  a - b;

subtract (0, 1)

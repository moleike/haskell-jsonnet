local x = 'foo';
local y = 'bar';

{ assert true, assert x == y, x: 1 } == { x: 1 }

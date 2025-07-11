import 'dart:math' as math;

var r = 3;

var area = math.pi * math.pow(r, 2);

var root = math.sqrt(49);

var sin45 = math.sin(math.pi / 4);

var log_e = math.log(math.e);

void main() {
  print(['Circle area with r =', r, '=>', area].join(' '));
  print(['Square root of 49:', root].join(' '));
  print(['sin(π/4):', sin45].join(' '));
  print(['log(e):', log_e].join(' '));
}

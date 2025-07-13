import 'dart:io';

String input1 = stdin.readLineSync() ?? '';

String input2 = stdin.readLineSync() ?? '';

void main() {
  print("Enter first input:");
  print("Enter second input:");
  print(["You entered:".toString(), input1.toString(), ",".toString(), input2.toString()].join(' '));
}

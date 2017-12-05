#include <iostream>
#include <vector>

using namespace std;

int steps_to_escape_part1(vector<int>& instructions) {
  int pos = 0, iterations = 0, new_pos;

  while (pos < instructions.size()) {
    new_pos = pos + instructions[pos];
    instructions[pos]++;
    pos = new_pos;
    iterations++;
  }

  return iterations;
}

int steps_to_escape_part2(vector<int>& instructions) {
  int pos = 0, iterations = 0, new_pos, offset;

  while (pos < instructions.size()) {
    offset = instructions[pos];
    new_pos = pos + offset;
    instructions[pos] += offset >=3 ? -1 : 1;
    pos = new_pos;
    iterations++;
  }

  return iterations;
}

int main() {
  vector<int> instructions1, instructions2;
  int n;

  while (true) {
    cin >> n;
    if (cin.eof())
      break;
    instructions1.push_back(n);
  }

  instructions2.insert(instructions2.begin(),
                       instructions1.begin(), instructions1.end());

  cout << steps_to_escape_part1(instructions1) << endl;
  cout << steps_to_escape_part2(instructions2) << endl;

  return 0;
}

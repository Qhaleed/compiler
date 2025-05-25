#include <iostream>
#include <string>
using namespace std;

void multiply(int x, int y);
void square(int num);

int main() {
    multiply(6, 7);
    square(8);
    return 0;
}
void multiply(int x, int y) {
    int result = (x * y);
    cout << "Result of multiplication:" << " " << result << endl;
}
void square(int num) {
    int squared = (num * num);
    cout << "Square of" << " " << num << " " << "is" << " " << squared << endl;
}

#include <iostream>
#include <string>
using namespace std;

void calculator(int a, int b);

int main() {
    calculator(20, 4);
    calculator(15, 3);
    return 0;
}
void calculator(int a, int b) {
    int sum = (a + b);
    int diff = (a - b);
    int prod = (a * b);
    int div = (a / b);
    cout << "Calculator for" << " " << a << " " << "and" << " " << b << endl;
    cout << "Addition:" << " " << sum << endl;
    cout << "Subtraction:" << " " << diff << endl;
    cout << "Multiplication:" << " " << prod << endl;
    cout << "Division:" << " " << div << endl;
}

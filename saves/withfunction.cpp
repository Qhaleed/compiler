#include <iostream>
#include <string>
using namespace std;

int add(int a, int b);

int main() {
    int number = 32;
    cout << number << " " << "test" << " " << 42 << endl;
    add(5, 3);
    return 0;
}
int add(int a, int b) {
    int sum = (a + b);
    cout << sum << endl;
}

#include <iostream>
#include <string>
using namespace std;

int add(int a, int b);

int main() {
    add(5, 3);
    return 0;
}
int add(int a, int b) {
    int sum = (a + b);
    cout << sum << endl;
}

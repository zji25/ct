#include <iostream>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, d;
    d = 2;
    cin >> n;
    while (n > 1) {
        while (n % d == 0) {
            cout << d << " ";
            n = n / d;
        }
        d++;
    }
}
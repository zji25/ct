#include <iostream>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, i;
    cin >> n;
    bool a[2 * 10000000 + 1];
    for (int i = 0; i < 2 * 10000000 + 1; i++) a[i] = true;
    a[1] = false;
    i = 2;
    for (int i = 2; i <= 2 * 10000000; i++) {
        if (a[i] != false && 1000000000 / i >= i) {
            for (int j = i * i; j < 2 * 10000000 + 1; j += i) a[j] = false;
        }
    }
    int x;
    for (int i = 0; i < n; i++) {
        cin >> x; 
        if (a[x] == 0) cout << "NO\n";
        else cout << "YES\n";
    }
}
#include <iostream>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);
    int n, k, count, lg;
    cin >> n >> k;
    int a[110];
    for (int i = 0; i < n; i++) {
        a[i] = 1;
    }
    for (int i = n; i < 110; i++) {
        a[i] = -1;
    }
    count = -1;
    lg = n;
    while (lg > 0) {
        count++;
        if (count == k) {
            cout << a[0];
            for (int i = 1; i < lg; i++) {
                cout << '+' << a[i];
            }
            return 0;
        }
        if (lg == 1) {
            return 0;
        }
        a[lg - 2]++;
        a[lg - 1]--;
        if (a[lg - 2] > a[lg - 1]) {
            a[lg - 2] += a[lg - 1];
            a[lg - 1] = -1;
            lg -= 1;
        } else {
            while (a[lg - 1] >= a[lg - 2] * 2) {
                a[lg] = a[lg - 1] - a[lg - 2];
                lg += 1;
                a[lg - 2] = a[lg - 3];
            }
        }
    }

    return 0;
}
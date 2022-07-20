#include <iostream>
#include <string>
 
using namespace std;
 
int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);
    int n, count, lg;
    string s;
    cin >> s;
    int a[110];
    int b[110];
    int last, counter;
    last = 0;
    counter = 0;
    n = 0;
    for (int i = 0; i < s.length(); i++) {
        if (s.at(i) == '+') {
            b[counter] = last;
            n += last;
            last = 0;
            counter++;
        } else {
            last = last * 10 + ((int) s.at(i) - 48);
        }
    }
    b[counter] = last;
    n += last;
    for (int i = counter + 1; i < 110; i++) {
        b[i] = -1;
    }
    for (int i = n; i < 110; i++) {
        a[i] = -1;
    }
    for (int i = 0; i < n; i++) {
        a[i] = 1;
    }
    count = -1;
    lg = n;
    while (lg > 0) {
        count++;
        bool flag;
        flag = true;
        for (int i = 0; i < 110; i++) {
            if (a[i] != b[i]) {
                flag = false;
                break;
            }
        }
        if (flag) {
            cout << count;
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
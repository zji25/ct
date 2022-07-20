#include <iostream>
#include <string>
#include <math.h>

using namespace std;

int bin_to_dec(string s9) {
    int value = 0;
    int index = 0;
    for (int i = s9.length() - 1; i>=0; i--) {
        if (s9.at(i) == '1') {
            value += pow(2, index);
        }
        index++;
    }
    return value;
}

int main(){
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, q;
    cin >> n;
    string function[n];
    string p;
    function[0] = "01";
    int depths[n];
    depths[0] = 1;
    int tablesize = 1;
    for (int o = 0; o < n; o++) {
        int amount;
        cin >> amount;
        if (amount == 0) {
            tablesize *= 2;
            if (o > 0) {
                for (int i = 0; i < o; i++){
                    while (function[i].length() < tablesize) {
                        string str = function[i];
                        string str2;
                        for (int j = 0; j < str.length(); j++) {
                            str2 += str[j];
                            str2 += str[j];
                        }
                        function[i] = str2;
                    }

                }
                string str3 = "01";
                while (str3.length() < function[0].length()) {
                    str3 = str3 + str3;
                }
                function[o] = str3;
                depths[o] = 1;
            }
        } else {
            int s[amount];
            for (int i = 0; i < amount; i++) {
                cin >> q;
                s[i] = q;
            }
            int power = pow(2, amount);
            string f[power];
            for (int i = 0; i < power; i++) {
                cin >> p;
                f[i] = p;
            }
            int max_of_depths = -1;
            for (int d = 0; d < amount; d++) {
                if (depths[s[d] - 1] > max_of_depths) {
                    max_of_depths = depths[s[d] - 1];
                }
            }
            depths[o] = max_of_depths + 1;
            string will_append;
            for (int i = 0; i < function[0].length(); i++) {
                string st;
                for (int j = 0; j < amount; j++) {
                    st += function[s[j] - 1][i];
                }
                will_append += f[bin_to_dec(st)];
            }
            function[o] = will_append;
        }

    }
    cout << (depths[n - 1] - 1) << endl;
    cout << function[n - 1];
    return 0;
}




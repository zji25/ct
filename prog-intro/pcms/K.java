package pcms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class K {
    public static int[][] matrix;
    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        int n, m, s, right, left, area, max_area = 0;
        String str;
        int a00 = 0, a01 = 0, a10 = 0, a11 = 0;
        int[][] letters;
        String[] string;
        string = reader.readLine().split(" ");
        n = Integer.parseInt(string[0]);
        m = Integer.parseInt(string[1]);
        matrix = new int[n][m];
        letters = new int[26][2];
        for (int i = 0; i < 26; i++) {
            letters[i][0] = -1;
            letters[i][1] = -1;
        }

        for (int i = 0; i < n; i++) {
            str = reader.readLine();
            for (int j = 0; j < m; j++) {
                s = str.charAt(j);
                matrix[i][j] = s;
                if (str.charAt(j) != 46) {
                    letters[s - 65][0] = i;
                    letters[s - 65][1] = j;
                }
            }
        }
        reader.close();
        boolean flag;
        for (int top = 0; top < n; top++) {
            for (int bot = 0; bot < n; bot++) {
                if (top <= letters[0][0] && letters[0][0] <= bot) { // A между top bottom
                    flag = true;
                    left = 0;
                    right = m - 1;
                    for (int i = 1; i < 26; i++) {
                        if (letters[i][0] != -1 && top <= letters[i][0] && letters[i][0] <= bot) {
                            if (letters[i][1] < letters[0][1] && left <= letters[i][1]) {
                                left = letters[i][1] + 1;
                            } else if (letters[i][1] > letters[0][1] && letters[i][1] <= right) {
                                right = letters[i][1] - 1;
                            } else if (letters[i][1] == letters[0][1]) {
                                flag = false;
                                break;
                            }
                        }
                    }
                    area = (right - left + 1) * (bot - top + 1);
                    if (flag && area > max_area) {
                        max_area = area;
                        a00 = top;
                        a01 = left;
                        a10 = bot;
                        a11 = right;
                    }
                }
            }
        }
        for (int i = a00; i < a10 + 1; i++) {
            for (int j = a01; j < a11 + 1; j++) {
                if (matrix[i][j] != 65) {
                    matrix[i][j] = 97;
                }
            }
        }

        if (a00 != 0) {
            fill(0, 0, a00, m);
        }
        if (a10 != n - 1) {
            fill(a10 + 1, 0, n, m);
        }
        if (a01 != 0) {
            fill(a00, 0, a10 + 1, a01);
        }
        if (a11 != m - 1) {
            fill(a00, a11 + 1, a10 + 1, m);
        }

        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                builder.append((char) matrix[i][j]);
            }
            System.out.println(builder);
            builder.setLength(0);
        }
    }

    public static void fill(int start_row, int start_col, int end_row, int end_col) {
        int lower;
        for (int i = start_row; i < end_row; i++) {
            for (int j = start_col; j < end_col; j++) {
                if (Character.isLetter(matrix[i][j]) && Character.isUpperCase(matrix[i][j])) {
                    lower = Character.toLowerCase((char) matrix[i][j]);
                    for (int k = i + 1; k < end_row; k++) {
                        if (matrix[k][j] == 46) {
                            matrix[k][j] = lower;
                        } else {
                            break;
                        }
                    }
                    for (int k = i - 1; k > start_row - 1; k--) {
                        if (matrix[k][j] == 46) {
                            matrix[k][j] = lower;
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        int start = start_col;
        int end = start_col;
        for (int i = start_row; i < end_row; i++) {
            for (int j = start_col; j < end_col; j++) {
                if (matrix[i][j] != 46) {
                    lower = Character.toLowerCase((char) matrix[i][j]);
                    for (int k = start; k < j; k++) {
                        matrix[i][k] = lower;
                    }
                    start = j + 1;
                    end = j + 1;
                }
            }
            start = start_col;
        }
        for (int i = start_row; i < end_row; i++) {
            lower = Character.toLowerCase((char) matrix[i][end - 1]);
            for (int j = end; j < end_col; j++) {
                matrix[i][j] = lower;
            }
        }
    }
}
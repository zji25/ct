package search;

// let SEARCH_INV be -1 <= l' < r' <= a.length
//    & a[l'] > x >= a[r']
//    (a[-1] considered max value; a[a.length] min value)

public class BinarySearch {

//  p: function got the required data types
//     & for each j in [1:args.length-2] a[j] >= a[j+1]
//  q: returns a value = a minimal index of a when x <= a[value]
    static int iterativeBinarySearch(int x, int[] a) {
        int l = -1;
        int r = a.length;
        int m;
//      l, r initialized with int values & [l' < r'];

//      invariant: SEARCH_INV
        while (r - l > 1) {
//        p0: invariant & [r'-l' > 1] |-> invariant & [r'-l' >= 2]
//          p1: invariant & [r'-l' >= 2]
            m = l + (r - l) / 2;
//          q1: invariant & m' = l'+(r'-l')/2 & [r'-l' >= 2] | ->
//              [l'+(r'-l')/2 >= l'+2/2 = l'+1 > l'] |->
//              [l' < m' <= r']
//              & for each n > 1: [l' = r'-n] [m' = l'+(l'+n-l')/2 = l'+n/2 < l'+n = r'] |->
//              [l' < m' < r']
            if (x >= a[m]) {
//              p2: invariant & [l' < m' < r'] & [x >= a[m]] |->
//                  a[l'] > x >= a[m'] >= a[r'] & [r'-l' > m'-l']
                r = m;
//              c2: r' = m'
//              q2: invariant & r' decreased |-> r'-l' decreased
            } else {
//              p3: invariant & [l' < m' < r'] & [a[m'] > x] |->
//                  [a[l'] >= a[m'] > x >= a[r']] & [r'-l' > r'-m']
                l = m;
//              c2: l' = m'
//              q3: invariant & l' increased |-> r'-l' decreased
            }
//        q0: invariant & r'-l' decreased
        }
//      q (for while loop): invariant & r'-l' <= 1 (!cond) |->
//          a[l'] > x >= a[r'] & l' = r'-1 |->
//          a[r'-1] > x >= a[r']

        return r;
//  q: a[r'-1] > x >= a[r'] |-> return value is a minimal index of a when x <= a[value]
//     & 0 <= value < a.length() if such value exists in a
//     & value = a.length() if a[a.length() - 1] > x (if it doesn't)
    }

//  p: function got the required data types
//     & for each j in [1:args.length-2] a[j] >= a[j+1]
//     & -1 <= l <= r <= a.length
//  q: returns a value = a minimal index of a when x <= a[value]
    static int recursiveBinarySearch(int x, int[] a, int l, int r) {
//  invariant: SEARCH_INV
        if (l >= r - 1) {
//          p: invariant & l' >= r'-1 & l' < r'|->
//             a[l'] > x >= a[r'] & l' = r'-1 |->
//             a[r'-1] > x >= a[r']
            return r;
//          q: return value is a minimal index of a when x <= a[value]
//             & 0 <= value < a.length() if such value exists in a
//             & value = a.length() if a[a.length() - 1] > x (if it doesn't)
        }
//      q: invariant & l' < r'-1

//      p: invariant & l' < r'-1 (|-> [r'-l' >= 2])
        int m = l + (r - l) / 2;
//      q: invariant & [m initialized with value: m = l'+(r'-l')/2] & [r'-l' >= 2] |->
//         [(r'-l')/2 >= 1] |-> [l'+(r'-l')/2 >= l'+1] |-> [l'+(r'-l')/2 > l'] |-> [m > l']
//         & for each n > 1: [l' = r'-n] [m = l'+(l'+n-l')/2 = l'+n/2 < l'+n = r'] |->
//         [l' < m < r'] |->
//         -1 <= l' < m < r' <= a.length
//         & a[l'] > x >= a[r']

        if (x >= a[m]) {
//          p: invariant & x >= a[m] & [m < r'] |->
//             [a[l'] > x >= a[m] >= a[r']] & [r'-l' > m-l']
            return recursiveBinarySearch(x, a, l, m);
//          c: r' = m
//          q: invariant & a[l'] > x >= a[r'] & r'-l' decreased
        } else {
//          p: invariant & [a[m] > x] & [l' < m] |->
//             [a[l'] >= a[m] > x >= a[r']] & [r'-l' > r'-m]
            return recursiveBinarySearch(x, a, m, r);
//          c: l' = m
//          q: invariant & a[l'] > x >= a[r'] & r'-l' decreased
        }
    }


//  p: args.length != 0
//     & input in a given format: String[] = ["y0", "y1", ..., "y(args.length-1)"]
//     & "" contain a number that can be parsed to int
//     & for i in args[1:args.length-2]  int(args[i]) >= int(args[i+1])
//  q: prints a value = min(S) where S is a set of all i in [1:args.length-1] & a[0] <= a[i]
    public static void main(String[] args) {
        int x = Integer.parseInt(args[0]);
        int[] a = new int[args.length - 1];
        int i = 1;
//      x, i initialized; a created: [x = (int) args[0]] & [a = int[args.length - 1]] & [i' = 1]

//      p (for while loop): args.length > 1
//         & input in a given format: String[] = ["y0", "y1", ..., "y(args.length-1)"]
//         & "" contain a number that can be parsed to int
//         & for i in args[1:args.length-1]  int(args[i-1]) >= int(args[i])
//      invariant: 1 <= i' <= args.length
        while (i < args.length) {
//          p: invariant & i' < args.length
            a[i - 1] = Integer.parseInt(args[i]);
//          c: a[i'-1] initialized with a number from args[i']
            i++;
//          c: increased value of i' by 1
//          q: invariant
        }
//      q (for while loop): invariant & i' = args.length
//         & for each j in [0:args.length-1] a[j] = (int) args[j+1]
//         & for each j in [0:args.length-2] a[j] >= a[j+1]

//      p0: [x = (int) args[0]] & [a = int[args.length - 1]]
//          & for each j in [0:args.length-1] a[j] = (int) args[j+1]
//          & for each j in [0:args.length-2] a[j] >= a[j+1]
        int ite = iterativeBinarySearch(x, a);
//      q0: ite is initialized with a returned value: a minimal index of a when x <= a[value]
//         & x' = x & for j in [0:args.length-1] a'[j] = a[j]

//      p1: [x = (int) args[0]] & [a = int[args.length - 1]]
//         & for each j in [0:args.length-1] a[j] = (int) args[j+1]
//         & for each j in [0:args.length-2] a[j] >= a[j+1]
        int rec = recursiveBinarySearch(x, a, -1, a.length);
//      q1: rec is initialized with a returned value: a minimal index of a when x <= a[value]
//         & x' = x & for j in [0:args.length-1] a'[j] = a[j]
//      q: ite = rec

//      to test both functions simultaneously:
        if (ite == rec) {
//          p: ite = rec <-> both functions returned a minimal index of a when x <= a[value]
            System.out.println(rec);
//          q: win
        }
//      q: win
    }
}

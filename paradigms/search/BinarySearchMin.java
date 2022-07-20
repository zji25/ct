package search;

// let MIN be first occurred in a min element's index

// let EXISTS_MIN a[0:a.length-1] be exists c: 0 <= c <= a.length - 1: for each j in [0:c-1] int(a[j]) > int(a[j+1]);
//     & exists c': 0 <= c' <= args.length - 1: for each j in [c':a.length-2] int(a[j]) < int(a[j+1])
//     & |c'-c| <= 1

// let SEARCH_INV be [0 <= l' <= r' <= a.length] & [a[l'] >= a[MIN] <= a[r']]
//     & for each i: i in range [0:(MIN-1)] a[i] > a[i+1]
//     for each j: j in range [(MIN):r-2] a[j] <= a[j+1]

public class BinarySearchMin {
    static int iterativeBinarySearch(int[] a) {
//      p: function got the required data type & a.length != 0
//         & for each i: i in range [0:(MIN-1)] a[i] > a[i+1]
//           for each j: j in range [(MIN):r-2] a[j] <= a[j+1]
        int l = 0;
        int r = a.length - 1;
//      l & r initialized with values: 0 <= l' <= r' < a.length
        int m;
//      invariant: SEARCH_INV
        while (r - l > 0) {
//        p: invariant & [r'-l' > 0] |-> [r'-l' >= 1]
            m = l + (r - l) / 2;
//          q0: invariant & [m' = l'+(r'-l')/2] |-> l' <= m'
//              & for each i > 0: [r'-l' = i] m' = l'+i/2 < l'+i = r' |-> m' < r'
//              |-> l' <= m' < r'

            if (a[m] > a[m + 1]) {
//              p1: invariant & l' <= m' < r' & a[m'] > a[m'+1]
//                  |-> a[l'] >= a[m'] > a[m'+1] >= a[MIN] <= a[r']
                l = m + 1;
//              q1: l'' := m'+1 > l' |-> a[l''] >= a[MIN] <= a[r'] & r'-l'' < r'-l'
//                  |-> invariant & r'-l' decreased
            } else {
//              p2: invariant & l' <= m' < r' & a[m'] <= a[m'+1]
//                  |-> a[l'] >= a[MIN] <= a[m'] <= a[m'+1] <= a[r']
                r = m;
//              q2: r'' := m' < r' |-> a[l'] >= a[MIN] <= a[r''] & r''-l' < r'-l'
//                  |-> invariant & r'-l' decreased
            }
//        q: invariant & r'-l' decreased
        }
//      q (for while loop): invariant & [r'-l' <= 0 (!cond)] & [r'-l' >= 0 (inv)] |-> r' = l' = MIN
        return a[l];
//      q (for function): returned value = a[MIN]
    }

    static int recursiveBinarySearch(int[] a, int l, int r) {
//    p: function got the required data type & a.length != 0
//       & for each i: i in range [0:(MIN-1)] a[i] > a[i+1]
//         for each j: j in range [(MIN):r-2] a[j] <= a[j+1]
//    invariant: SEARCH_INV
        if (l == r) {
//          p0: p & [l == r]
            return a[l];
//          q0: a[l] = a[MIN]
        }
//      p1: invariant & [l != r] & [l <= r] (inv) |-> [0 <= l < r <= a.length] & [a[l] >= a[MIN] <= a[r]]
        int m = l + (r - l) / 2;
//      q1: invariant & [m = l+(r-l)/2] |-> l <= m
//          & for each i > 0: [r-l = i] m = l+i/2 < l+i = r |-> m < r
//          |-> l <= m < r
        if (a[m] > a[m + 1]) {
//          p2: invariant & l <= m < r & a[m] > a[m+1]
//              |-> a[l] >= a[m] > a[m+1] >= a[MIN] <= a[r]
            return recursiveBinarySearch(a, m + 1, r);
//          q2: l' = m + 1 > l |-> a[l'] >= a[MIN] <= a[r] & r-l' < r-l
//              |-> invariant & r-l decreased
        } else {
//          p2: invariant & l <= m < r & a[m] <= a[m+1]
//              |-> a[l] >= a[MIN] <= a[m] <= a[m+1] <= a[r]
            return recursiveBinarySearch(a, l, m);
//          q2: r' := m < r |-> a[l] >= a[MIN] <= a[r'] & r'-l < r-l
//              |-> invariant & r-l decreased
        }
    }

//  p: args.length != 0
//     & input in a given format: String[] = ["y0", "y1", ..., "y(args.length-1)"]
//     & "" contain a number that can be parsed to int
//     & EXISTS_MIN args[0:args.length-1]
//  q: prints a value = a[MIN]
    public static void main(String[] args) {
        int[] a = new int[args.length];
        int i = 0;
//      i initialized; a created:  [a = int[args.length - 1]] & [i' = 0]

//      p (for while loop): args.length > 0
//         & input in a given format: String[] = ["y0", "y1", ..., "y(args.length-1)"]
//         & "" contain a number that can be parsed to int
//         & EXISTS_MIN args[0:args.length-1]
//      invariant: 0 <= i' <= args.length
        while (i < args.length) {
//          p: invariant & i' < args.length
            a[i] = Integer.parseInt(args[i]);
//          c: a[i'] initialized with a number from args[i']
            i++;
//          c: increased value of i' by 1
//          q: invariant
        }
//      q (for while loop): invariant & i' = args.length
//         & for each j in [0:args.length-1] a[j] = (int) args[j]
//         & EXISTS_MIN args[0:args.length-1]
//      p1: (= q for while loop)
        int ite = iterativeBinarySearch(a);
//      q1: ite is initialized with a returned value: min element of a
//         & for j in [0:args.length-1] a'[j] = a[j]

//      p2: (= q for while loop)
        int rec = recursiveBinarySearch(a, 0, a.length - 1);
//      q2: rec is initialized with a returned value: min element of a
//         & for j in [0:args.length-1] a'[j] = a[j]
//    q: ite = rec = a[MIN]

//      to test both functions simultaneously:
        if (ite == rec) {
//          p: ite = rec <-> both functions returned a value = a[MIN]
            System.out.println(rec);
//          q: win
        }
//      q: win
    }
}
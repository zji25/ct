/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Yarunina Xenia
 */
class Solution : MonotonicClock {
    private var c1 by RegularInt(0)
    private var c2 by RegularInt(0)
    private var c3 by RegularInt(0)
    private var s1 by RegularInt(0)
    private var s2 by RegularInt(0)
    private var s3 by RegularInt(0)

    override fun write(time: Time) {
        s1 = time.d1
        s2 = time.d2
        s3 = time.d3
        c3 = s3
        c2 = s2
        c1 = s1
    }

    override fun read(): Time {
        val c = Time(c1, c2, c3)
        val z3 = s3
        val z2 = s2
        val z1 = s1
        val s = Time(z1, z2, z3)
        if (c.compareTo(s) == 0) return c
        if (c.d1 == s.d1) {
            if (c.d2 == s.d2) return s
            return Time(s.d1, s.d2, 0)
        }
        return Time(s.d1, 0, 0)
    }
}
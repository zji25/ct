import kotlinx.atomicfu.*

class IntIntHashMap {
    private val core = atomic(Core(INITIAL_CAPACITY))

    operator fun get(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return toValue(core.value.getInternal(key))
    }
    fun put(key: Int, value: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        require(isValue(value)) { "Invalid value: $value" }
        return toValue(putAndRehashWhileNeeded(key, value))
    }
    fun remove(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return toValue(putAndRehashWhileNeeded(key, DEL_VALUE))
    }
    private fun putAndRehashWhileNeeded(key: Int, value: Int): Int {
        while (true) {
            val cv = core.value
            val old = cv.putInternal(key, value)
            if (old != NEEDS_REHASH) return old
            core.compareAndSet(cv, cv.rehash())
        }
    }

    private class Core(capacity: Int) {
        val map = AtomicIntArray(2*capacity)
        val shift: Int
        val next = atomic<Core?>(null)
        init {
            val mask = capacity - 1
            assert(mask > 0 && mask and capacity == 0) { "Capacity must be power of 2: $capacity" }
            shift = 32 - Integer.bitCount(mask)
        }

        fun getInternal(key: Int): Int {
            var i = index(key)
            var probes = 0
            while (true) {
                var mk = map[i].value
                while (mk != key) {
                    if (mk == NULL_KEY) return NULL_VALUE
                    if (++probes >= MAX_PROBES) return NULL_VALUE
                    if (i == 0) i = map.size
                    i -= 2
                    mk = map[i].value
                }
                val mv = map[i+1].value
                val nxt = next.value
                return when {
                    mv == MOV_VALUE -> nxt!!.getInternal(key)
                    mv < 0 -> {
                        nxt!!.move(mk, -mv)
                        casV(i, mv, MOV_VALUE)
                        nxt.getInternal(key)
                    }
                    else -> mv
                }
            }
        }
        fun putInternal(key: Int, value: Int): Int {
            var i = index(key)
            var probes = 0
            while (true) {
                val mk = map[i].value
                val mv = map[i+1].value
                val nxt = next.value
                when (mk) {
                    NULL_KEY -> return when {
                        value==DEL_VALUE -> NULL_VALUE
                        setKV(i, key, mv, value) -> mv
                        else -> continue
                    }
                    key -> return when {
                        mv == MOV_VALUE -> nxt!!.putInternal(key, value)
                        mv < 0 -> {
                            nxt!!.move(mk, -mv)
                            casV(i, mv, MOV_VALUE)
                            nxt.putInternal(key, value)
                        }
                        casV(i, mv, value) -> mv
                        else -> continue
                    }
                }
                if (++probes >= MAX_PROBES) return if (value == DEL_VALUE) NULL_VALUE else NEEDS_REHASH
                if (i == 0) i = map.size
                i -= 2
            }
        }
        fun rehash(): Core {
            next.compareAndSet(null, Core(map.size))
            val nxt = next.value!!
            var i = 0
            while (i < map.size) {
                while (true) {
                    val mk = map[i].value
                    val mv = map[i+1].value
                    when {
                        mv == MOV_VALUE -> break
                        mv < 0 -> if (nxt.move(mk, -mv)) if (casV(i, mv, MOV_VALUE)) break
                        mv == DEL_VALUE -> if (casV(i, mv, MOV_VALUE)) break
                        else -> if (casV(i, mv, -mv)) if (nxt.move(mk, mv)) if (casV(i, -mv, MOV_VALUE)) break
                    }
                }
                i += 2
            }
            return nxt
        }
        private fun move(key: Int, value: Int) : Boolean {
            var i = index(key)
            var probes = 0
            while (true) {
                val mk = map[i].value
                when (mk) {
                    NULL_KEY -> if (setKV(i, key, NULL_VALUE, value)) return true else continue
                    key -> {
                        casV(i, NULL_VALUE, value)
                        return true
                    }
                }
                if (++probes >= MAX_PROBES) return false
                if (i == 0) i = map.size
                i -= 2
            }
        }

        private fun casV(i: Int, e: Int, v: Int) : Boolean = map[i+1].compareAndSet(e, v)
        private fun setKV(i: Int, k: Int, ev: Int, v: Int) = map[i].compareAndSet(NULL_KEY, k) && map[i+1].compareAndSet(ev, v)

        fun index(key: Int): Int = (key * MAGIC ushr shift) * 2
    }
}

private const val MAGIC = -0x61c88647 // golden ratio
private const val INITIAL_CAPACITY = 2 // !!! DO NOT CHANGE INITIAL CAPACITY !!!
private const val MAX_PROBES = 8 // max number of probes to find an item
private const val NULL_KEY = 0 // missing key (initial value)
private const val NULL_VALUE = 0 // missing value (initial value)
private const val DEL_VALUE = Int.MAX_VALUE // mark for removed value
private const val NEEDS_REHASH = -1 // returned by `putInternal` to indicate that rehash is needed
private const val MOV_VALUE = Int.MIN_VALUE

private fun isValue(value: Int): Boolean = value in (1 until DEL_VALUE)
private fun toValue(value: Int): Int = if (isValue(value)) value else 0
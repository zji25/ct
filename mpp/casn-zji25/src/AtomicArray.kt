import kotlinx.atomicfu.*

class AtomicArray<E>(size: Int, initialValue: E) {
    private val a = Array<Ref<E>>(size) { Ref(initialValue) }
    fun get(index: Int) =
        a[index].value
    fun set(index: Int, value: E) {
        a[index].value = value
    }
    fun cas(index: Int, expect: E, update: E): Boolean = a[index].cas(expect, update)
    fun cas2(index1: Int, expect1: E, update1: E,
             index2: Int, expect2: E, update2: E): Boolean {
        if (index1 == index2) return expect1 == expect2 && cas(index2, expect2, update2)
        return if (index1 < index2) cc(index1, expect1, update1, index2, expect2, update2)
        else cc(index2, expect2, update2, index1, expect1, update1)
    }
    private fun cc(index1: Int, expect1: E, update1: E,
                   index2: Int, expect2: E, update2: E): Boolean {
        val desc = CAS2Descriptor(a[index1], expect1, update1, a[index2], expect2, update2)
        if (!a[index1].cas(expect1, desc)) return false
        return desc.complete()
    }
}

class Ref<E>(initialValue: E) {
    val v = atomic<Any?>(initialValue)
    var value: E
        get() {
            v.loop { cur ->
                when (cur) {
                    is Descriptor -> cur.complete()
                    else -> return cur as E
                }
            }
        }
        set(upd) {
            v.loop { cur ->
                when (cur) {
                    is Descriptor -> cur.complete()
                    else -> if (v.compareAndSet(cur, upd)) return
                }
            }
        }
    fun cas(expect: Any?, update: Any?): Boolean {
        v.loop { cur ->
            when (cur) {
                is Descriptor -> cur.complete()
                expect -> if (v.compareAndSet(cur, update)) return true
                else -> return false
            }
        }
    }
}

abstract class Descriptor { abstract fun complete() : Boolean }
class DCSSDescriptor<E>(val b: Ref<E>, val expect: E, val update: Any?, val desc: CAS2Descriptor<E>) : Descriptor() {
    val outcome = atomic<Boolean?>(null)
    override fun complete() : Boolean {
        val out = desc.outcome.value == null
        outcome.compareAndSet(null, out)
        val ov = outcome.value!!
        val upd = if (ov) update else expect
        b.v.compareAndSet(this, upd)
        return ov
    }
}
class CAS2Descriptor<E>(val a: Ref<E>, val expectA: E, val updateA: E,
                        val b: Ref<E>, val expectB: E, val updateB: E) : Descriptor() {
    val outcome = atomic<Boolean?>(null)
    override fun complete() : Boolean {
        outcome.compareAndSet(null, out())
        val ov = outcome.value!!
        val aa = if (ov) updateA else expectA
        val bb = if (ov) updateB else expectB
        a.v.compareAndSet(this, aa)
        b.v.compareAndSet(this, bb)
        return ov
    }
    private fun out() : Boolean {
        val desc = DCSSDescriptor(b, expectB, this, this)
        if (b.v.value?.equals(this) == true) return true
        if (!b.cas(expectB, desc)) return false
        return desc.complete()
    }
}

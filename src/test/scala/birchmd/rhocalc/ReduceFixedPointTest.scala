package birchmd.rhocalc

object ReduceFixedPointTest {
  def main(args: Array[String]): Unit = {
    val x: Name = Zero.name
    
    def d(x: Name): Process = InputGuardedProcess(
      Seq(x), { case Seq(y) => OutputProcess(x, y).par(y.reify) }
    )

    val p = {
      val q = d(x)
      OutputProcess(x, q.name).par(q)
    }

    assert(p.reduce == p)
  }
}

package birchmd.rhocalc

sealed trait Name {
  def reify: Process
}

case class ProcessName(p: Process) extends Name {
  override def reify: Process = p

  override def toString: String = s"@(${p.toString})"
}

case class PlaceHolderName(n: String) extends Name {
  override def reify: Process = PlaceHolderProcess(n)

  override def toString: String = n
}

package birchmd.rhocalc

sealed trait Process {
  def name: Name = ProcessName(this)
 
  def threads: Seq[Process] = Seq(this)

  def par(other: Process): Process = ParProcess(threads ++ other.threads)

  def reduce: Process = this
}

case object Zero extends Process {
  override def toString: String = "0"
}

case class InputGuardedProcess(inputNames: Seq[Name], 
                               continuation: (Seq[Name]) => Process)
                               extends Process {
  override def toString: String = {
    val placeholders = inputNames.iterator
      .zip(InputGuardedProcess.placeholderNames.iterator)
      .map{ case (_, v) => PlaceHolderName(v) }
      .toSeq
 
    val forContent = inputNames.iterator
      .zip(placeholders.iterator)
      .map{ case (a, b) => s"$b <- $a" }
     
    s"for(${forContent.mkString(";")}).(${continuation(placeholders).toString}) "
  }

  def outputMatch(outputs: Seq[OutputProcess]): 
      Option[(InputGuardedProcess, Process)] = {
    if (this.inputNames.forall(n => outputs.exists(_.channel == n))) {
      Some(this -> continuation(inputNames.map(n => {
        outputs.find(_.channel == n).map(_.value).get
      })))
    } else {
      None
    }
  }
}

object InputGuardedProcess {
  val placeholderNames: Seq[String] = Seq(
    "x", "y", "z", "u", "v", "w", "a", "b", "c", "d", "e", "f", "g", "h"
  )
}

case class SelectedInputProcess(options: Seq[InputGuardedProcess]){
  override def toString: String = s"(${options.mkString(" + ")})"
}

case class OutputProcess(channel: Name, value: Name) extends Process {
  override def toString: String = s"$channel!($value)"
}

case class ParProcess(private val _threads: Seq[Process]) extends Process {
  override def threads: Seq[Process] = _threads

  override def reduce: Process = {
    val outputs = _threads.iterator
      .filter(_.isInstanceOf[OutputProcess])
      .map(_.asInstanceOf[OutputProcess])
      .toSeq

    val inputs = _threads.flatMap{
      case p: InputGuardedProcess => p.outputMatch(outputs)
      case q: SelectedInputProcess =>
        q.options.flatMap(_.outputMatch(outputs)).headOption
      case _ => None
    }

    val usedOutputs = inputs.iterator.flatMap(_._1.inputNames).toSet

    val newThreads = _threads.flatMap{
      case p: InputGuardedProcess =>
        inputs.find(_._1 == p).map(_._2).orElse(Some(p))
      
      case q: OutputProcess => 
        if (usedOutputs.contains(q.channel)) None else Some(q)

      case other => Some(other)
    }

    if (newThreads.size == 1) {
      newThreads.head
    } else {
      ParProcess(newThreads)
    }
  }

  override def toString: String = _threads.mkString(" | ")
}

case class PlaceHolderProcess(n: String) extends Process {
  override def name: Name = PlaceHolderName(n)

  override def toString: String = s"*$n"
}

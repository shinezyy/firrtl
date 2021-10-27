package firrtl.transforms

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.passes.memlib.DefAnnotatedMemory
import firrtl.passes.{CommonSubexpressionElimination, SplitExpressions}

case class DumperAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  def duplicate(newTarget: ModuleTarget) = this.copy(target = newTarget)
}

class DumperTransform() extends Transform with DependencyAPIMigration {

  override def prerequisites = Seq(
    Dependency[Flatten],
    Dependency[DeadCodeElimination],
    Dependency(CommonSubexpressionElimination),
    Dependency(SplitExpressions)
  )
  override def invalidates(a: Transform) = a match {
//    case x: VerilogEmitter => true
    case x => false
  }

  override def execute(state: CircuitState): CircuitState = {
    println("Starting transform 'DumperTransform'")

    val dumpMod = scala.collection.mutable.ListBuffer[String]()
    val annotationsx = state.annotations.flatMap {
      case DumperAnnotation(a: ModuleTarget) =>
        println(s"  - Module '${a.serialize}' annotated with dumper")
        dumpMod += a.name
        None
      case a =>
        Some(a)
    }

    val circuit = state.circuit
    val circuitx = circuit.mapModule {
      case m @ Module(_, name, ports, body) if dumpMod.contains(name) =>
        println("adding print to " + name)
        val clk = m.ports.collect {
          case x @ Port(_, "clock", _, _) => x
        }.head

        val inputs = m.ports.filter(_.direction == Input).filter(_.name != "clock")
        val outputs = m.ports.filter(_.direction == Output)

        val mems = scala.collection.mutable.ListBuffer[String]()
        m.foreachStmt {
          case Block(stmts) =>
            stmts.foreach {
              case DefRegister(info, name, tpe, clock, reset, init) =>
//                println("reg:", name, tpe, info)
              case DefMemory(info, name, dataType, depth, _, _, _, _, _, _) if info.toString.contains("SRAMTemplate")=>
                println("mem:", name, dataType, depth, info)
                mems += name
              case x => Unit
            }
          case x => Unit
        }

        val ports = inputs ++ outputs
        val portNames = ports.map(x => (if (x.direction == Input) "in_" else "out_") + x.name)

        println("Signal list:")
        println((portNames ++ mems.map("mem_" + _)).reduce(_ + " " + _))

        println("Ram list:")
//        println(mems.map(_ + ".ram").reduce(_ + ",\n" + _))

        m.mapStmt {
          case Block(stmts) =>
            Block(stmts :+ new Print(new FileInfo("dumper"),
              StringLit(("%b," * (ports.length + mems.length * 7)).dropRight(1) + "\n"),
              ports.map(Reference(_)), // ++ mems.map(s => new Reference(s + ".ram", UIntType(IntWidth(1)), MemKind, SourceFlow)),
              Reference(clk), UIntLiteral(1)))
          case x => x
        }
      case x => x
    }

    state.copy(annotations = annotationsx, circuit = circuitx)
  }
}


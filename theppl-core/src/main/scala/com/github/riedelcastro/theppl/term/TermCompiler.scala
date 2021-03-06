package com.github.riedelcastro.theppl.term

import com.github.riedelcastro.theppl.{Variable, State, IntVar}
import javax.tools._
import java.net.URI
import javax.tools.JavaFileObject.Kind
import java.util
import collection.JavaConversions._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import javax.tools.JavaFileManager.Location
import collection.mutable

trait CompiledState {
  def setState(state: State)

}

/**
 * http://stackoverflow.com/questions/2946338/how-do-i-programmatically-compile-and-instantiate-a-java-class
 * @author Sebastian Riedel
 */
object TermCompiler {

  val packageName = "just.generated"

  def indent(count: Int)(text: String) = Array.fill(count)(" ").mkString + text

  case class CompilationSource(stateSource: String, termSource: String)

  class CompilationContext(val term: Term[Any], val termName: String) {
    val compiledStateClassName = "CompiledStateFor" + termName
    val compiledTermClassName = "CompiledTermFor" + termName
    val cache = new VarNameCache
    val variables = term.variables.toSeq
    val index2variable = variables.zipWithIndex.toMap
    val indices = depthFirst(term).collect({ case Indexed(index, _) => index })
    val constants = new Pool(depthFirst(term).filter(javaTypeHelper(_).typ == "Object").collect({ case Constant(value) => value }), "_constant" + _)
    val indexNames = indices.zipWithIndex.map(pair => pair._1 -> ("_index" + pair._2))
    val index2Name = indexNames.toMap
  }

  class Pool[T](val elements:Seq[T], naming:Int => String) {
    val value2index = elements.zipWithIndex.toMap
    val value2name = elements.zipWithIndex.toMap.mapValues(naming(_))
    def unified = value2index.keys
  }

  class VarNameCreator {
    val index = 0
    val counts = new mutable.HashMap[String, Int]()
    def createName(v: Variable[Any]) = {
      val count = counts.getOrElse(v.stringRepr, 0)
      val name = v.stringRepr + count
      counts(v.stringRepr) = count + 1
      name
    }
  }

  class VarNameCache {
    val creator = new VarNameCreator
    val cache = new mutable.HashMap[Variable[Any], String]()
    def getName(variable: Variable[Any]) = {
      cache.getOrElseUpdate(variable, creator.createName(variable))
    }
  }

  def depthFirst(term: Term[Any]): Seq[Term[Any]] = {
    term match {
      case c: Composite[_, _] => term +: c.parts.flatMap(depthFirst(_))
      case _ => Seq(term)
    }
  }

  def createCompiledTermSource[V](implicit context: CompilationContext) = {
    import context._

    val helper = javaTypeHelper(term)

    val compiledExpression = compileTerm(term, "state")

    val indices = for ((_, name) <- indexNames) yield {
      indent(4) { "public final Index %s;".format(name) }
    }

    val indexInits = for (((_, name), number) <- indexNames.zipWithIndex) yield {
      indent(8) { "%s = context.indices().apply(%d);".format(name, number) }
    }

    val constants = for (value <- context.constants.unified) yield {
      indent(4) { "private final Object %s;".format(context.constants.value2name(value)) }
    }

    val constantInits = for (value <- context.constants.unified) yield {
      indent(8) { "%s = context.constants().elements().apply(%d);".format(
        context.constants.value2name(value), context.constants.value2index(value)) }
    }

    val source =
      """
        |package %s;
        |
        |import com.github.riedelcastro.theppl.term.*;
        |import com.github.riedelcastro.theppl.term.TermCompiler.*;
        |import com.github.riedelcastro.theppl.*;
        |import scala.collection.*;
        |import scala.Option;
        |
        |public class %s implements CompilationResult<%s>{
        |
        |    CompilationContext context;
        |
        |%s
        |
        |    public %s(CompilationContext context) {
        |        this.context = context;
        |
        |%s
        |    }
        |
        |    public final %s compiledEval(%s state) {
        |        return %s;
        |    }
        |
        |    public %s eval(State state) {
        |        %s compiledState = new %s(context);
        |        compiledState.setState(state);
        |        %s compiledResult = compiledEval(compiledState);
        |        return %s;
        |    }
        |
        |}
      """.stripMargin.format(
        packageName,
        compiledTermClassName, helper.boxedType,
        (indices++constants).mkString("\n"),
        compiledTermClassName,
        (indexInits++constantInits).mkString("\n"),
        helper.typ, compiledStateClassName,
        compiledExpression,
        helper.boxedType,
        compiledStateClassName, compiledStateClassName,
        helper.typ,
        helper.box("compiledResult")
      )
    println(source)
    source
  }


  def createCompiledStateSource(implicit context: CompilationContext) = {
    import context._
    //create state data structure:
    //find free variables
    //we create a java class that has one member variable per variable term
    val members = for (v <- variables) yield {
      val name = cache.getName(v)
      val helper = javaTypeHelper(v)
      indent(4) { "public %s %s = %s;".format(helper.typ, name, v.default) }
    }



    //member initialization
    val inits = for (v <- variables) yield {
      val helper = javaTypeHelper(v)
      val varIndex = index2variable(v)
      val option = indent(12) { "Option tmp = state.get(context.variables().apply(%d));".format(varIndex) }
      val ifDefined = indent(12) {
        "if (tmp.isDefined()) %s = %s;".format(
          cache.getName(v),
          helper.unbox("((%s)tmp.get())".format(helper.boxedType)))
      }
      Seq(indent(8) { "{" }, option, ifDefined, indent(8) { "}" }).mkString("\n")
    }


    val stateSource =
      """
        |package %s;
        |
        |import com.github.riedelcastro.theppl.*;
        |import com.github.riedelcastro.theppl.term.*;
        |import com.github.riedelcastro.theppl.term.TermCompiler.*;
        |import scala.collection.*;
        |import scala.Option;
        |
        |public class %s implements CompiledState {
        |
        |    CompilationContext context;
        |
        |    public %s(CompilationContext context) {
        |        this.context = context;
        |    }
        |
        |%s
        |
        |    public void setState(State state) {
        |%s
        |    }
        |
        |};
      """.stripMargin.format(packageName, compiledStateClassName, compiledStateClassName, members.mkString("\n"), inits.mkString("\n"))
    stateSource

  }

  trait JavaTypeHelper[+V] {
    def typ: String
    def boxedType: String = typ
    def unbox(expression: String): String
    def box(expression: String): String
  }

  def javaTypeHelper(v: Term[Any]): JavaTypeHelper[Any] = {
    v.default match {
      case i: Int => new JavaTypeHelper[Int] {
        def typ = "int"
        override def boxedType = "Integer"
        def unbox(expression: String) = expression + ".intValue()"
        def box(expression: String) = "Integer.valueOf(%s)".format(expression)
      }
      case d: Double => new JavaTypeHelper[Double] {
        def typ = "double"
        override def boxedType = "Double"
        def unbox(expression: String) = expression + ".doubleValue()"
        def box(expression: String) = "Double.valueOf(%s)".format(expression)
      }
      case b: Boolean => new JavaTypeHelper[Boolean] {
        def typ = "boolean"
        override def boxedType = "Boolean"
        def unbox(expression: String) = expression + ".booleanValue()"
        def box(expression: String) = "Boolean.valueOf(%s)".format(expression)
      }
      case x => new JavaTypeHelper[Any] {
        def typ = "Object"
        def unbox(expression: String) = expression
        def box(expression: String) = expression
      }
    }
  }


  def compileTerm(t: Term[Any], stateVarName: String)(implicit context: CompilationContext): String = {
    val helper = javaTypeHelper(t)
    t match {
      case Constant(v) =>
        if (helper.typ != "Object") v.toString else context.constants.value2name(v)

      case SingletonVecTerm(index,value) =>
        "new SingletonVec(%s,%s)".format(compileTerm(index,stateVarName),compileTerm(value,stateVarName))

      case Indexed(index, key) =>
        val args = key.args.map(compileTerm(_, stateVarName)).mkString("new Object[] {", ",", "}")
        "%s.index(%s)".format(context.index2Name(index), args)

      case SingletonVector(args, value) =>
        "" //todo: becomes term.indices(new Object[]{boxed(args(0)),...}):int -> value:double

      case v: Variable[_] =>
        stateVarName + "." + context.cache.getName(v)

      case Applied1(f: UnaryFun[_, _], arg1) =>
        f.javaExpr(compileTerm(arg1, stateVarName))

      case Applied2(f: InfixFun[_, _, _], arg1, arg2) =>
        f.javaExpr(compileTerm(arg1, stateVarName), compileTerm(arg2, stateVarName))

      case _ => sys.error("Cannot compile " + t)
    }
  }

  def compile[V](term: Term[V]): CompiledTerm[V] = {
    implicit val context = new CompilationContext(term, "Term")
    import context._
    val qualified = packageName + "." + compiledTermClassName
    val stateSource = createCompiledStateSource
    val termSource = createCompiledTermSource
    val compiledStateSource = SourceJavaFileObject(compiledStateClassName + ".java", stateSource)
    val compiledTermSource = SourceJavaFileObject(compiledTermClassName + ".java", termSource)

    println(stateSource)

    val compiler = ToolProvider.getSystemJavaCompiler()
    val diagnosticsCollector = new DiagnosticCollector[JavaFileObject]()
    val fileManager = compiler.getStandardFileManager(diagnosticsCollector, null, null)
    val jfm = new RAMFileManager(fileManager)

    val task = compiler.getTask(null, jfm, diagnosticsCollector, null, null,
      util.Arrays.asList(compiledStateSource, compiledTermSource))
    val result = task.call()
    println(result)

    for (d <- diagnosticsCollector.getDiagnostics) {
      println(d)
      println(d.getCode)
      println(d.getSource.getCharContent(true).toString.split("\n")(d.getLineNumber.toInt - 1))
    }

    //create term
    val clazz = Class.forName(qualified, false, jfm.loader)
    val compiledState = clazz.getDeclaredConstructor(classOf[CompilationContext]).
      newInstance(context).asInstanceOf[CompilationResult[V]]

    println(compiledState)

    CompiledTerm(compiledState, context, CompilationSource(stateSource, termSource))
  }

  def main(args: Array[String]) {
    import TermImplicits._
    val test = IntVar('test)
    val compiled = compile(I { (test + test) === 2 })
    println("*** Eval:" + compiled.eval(State(Map(test -> 1))))
    println("*** Eval:" + compiled.eval(State(Map(test -> 2))))

    println(compiled)
  }

}

class RAMFileManager(sjfm: StandardJavaFileManager)
  extends ForwardingJavaFileManager[StandardJavaFileManager](sjfm) {

  var cache: Map[String, RAMJavaFileObject] = Map.empty

  val loader = new RAMClassLoader

  class RAMClassLoader extends ClassLoader {
    override def findClass(name: String) = {
      val result = cache.get(name).map(jfo => {
        val bytes = jfo.byteArrayOutStream.toByteArray
        defineClass(name, bytes, 0, bytes.length)
      })
      result.getOrElse(super.findClass(name))
    }
  }

  override def getJavaFileForOutput(location: Location, name: String, kind: Kind, sibling: FileObject) = {
    val jfo = new RAMJavaFileObject(name, kind)
    cache += name -> jfo
    jfo
  }
  override def getClassLoader(loc: Location) = loader
  override def inferBinaryName(loc: Location, jfo: JavaFileObject) = {
    if (loc == StandardLocation.CLASS_PATH && jfo.isInstanceOf[RAMJavaFileObject]) jfo.getName
    else super.inferBinaryName(loc, jfo)
  }
  override def list(loc: Location, pkg: String, kind: util.Set[Kind], recurse: Boolean) = {
    val result = super.list(loc, pkg, kind, recurse).toSeq
    if (loc == StandardLocation.CLASS_PATH && pkg == TermCompiler.packageName && kind(JavaFileObject.Kind.CLASS)) {
      result ++ cache.values
    } else
      result
  }
}


case class SourceJavaFileObject(name: String, content: String) extends SimpleJavaFileObject(new URI(name), Kind.SOURCE) {
  override def getCharContent(ignoreEncodingErrors: Boolean) = content
}


/**
 * A JavaFileObject that uses RAM instead of disk to store the file. It
 * gets written to by the compiler, and read from by the loader.
 */
class RAMJavaFileObject(name: URI, kind: Kind) extends SimpleJavaFileObject(name, kind) {

  private var baos: ByteArrayOutputStream = null

  def this(name: String, kind: Kind) {
    this(new URI(name), kind)
  }

  def byteArrayOutStream = baos

  override def openInputStream() = {
    new ByteArrayInputStream(baos.toByteArray)
  }
  override def openOutputStream() = {
    baos = new ByteArrayOutputStream()
    baos
  }
  override def getCharContent(p1: Boolean) = {
    throw new UnsupportedOperationException();
  }
}


trait CompilationResult[+V] {

  def eval(state: State): V

}

case class CompiledTerm[+V](result: CompilationResult[V],
                            context: TermCompiler.CompilationContext,
                            source: TermCompiler.CompilationSource) extends Term[V] {
  def eval(state: State) = Some(result.eval(state))
  def variables = context.term.variables
  def default = context.term.default.asInstanceOf[V]
}

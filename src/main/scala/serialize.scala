package serialize

import tools.nsc.util.ScalaClassLoader
import java.io.{ObjectStreamClass, ObjectInputStream, ObjectOutputStream, IOException}

class InterpreterObjectOutputStream(delegate : ObjectOutputStream) extends ObjectOutputStream(delegate) {
  // TODO(joshuasuereth): Is this necessary?
  lazy val classLoaderClass : Option[Class[_]] = try {
      Some(Class.forName("scala.tools.nsc.interpreter.AbstractFileClassLoader"))
  } catch {
    case ex : ClassNotFoundException => None
  }
  private def isReplClassLoader(cl : ClassLoader) = {
    if (cl == null || cl.getClass == null) false
    else if (cl.getClass.getName == "scala.tools.nsc.interpreter.AbstractFileClassLoader") true
    else classLoaderClass.map(_.isAssignableFrom(cl.getClass)) getOrElse false
  }
  @throws(classOf[IOException])
  override protected def annotateClass(cl : Class[_]) : Unit = {
    val extramagic = isReplClassLoader(cl.getClassLoader)
    // Mark whether we had to serialize the class.
    writeBoolean(extramagic)
    if (extramagic) {
      val cll = cl.getClassLoader.asInstanceOf[ScalaClassLoader]
      // TODO(joshuasuereth): Figure out why this isn't working.
      val bytes = cll.findBytesForClassName(cl.getCanonicalName)
      Console.println("Serializing " + cl.getName + " with size = " + bytes.length + " from cl " + cll)
      if (bytes.length == 0) throw new IOException("Could not find REPL class for: " + cl.getCanonicalName)
      writeInt(bytes.length)
      write(bytes)
    }
  }
}
// A class loader the can only load a single class.
class SingleClassLoader(parent : ClassLoader, className : String, bytes : Array[Byte]) extends ClassLoader(parent) {
  @throws(classOf[ClassNotFoundException])
  override protected def findClass(name : String ) : Class[_] = if (name == className) {
    defineClass(name, bytes, 0, bytes.length)
  } else super.findClass(name)
  @throws(classOf[ClassNotFoundException])
  override def loadClass(name : String) : Class[_] = {
    val cl = findLoadedClass(name)
    if (cl == null) findClass(name) else cl
  }
}

class InterpreterObjectInputStream(delegate : ObjectInputStream) extends ObjectInputStream(delegate) {
  @throws(classOf[IOException]) @throws(classOf[ClassNotFoundException])
  protected override def resolveClass(desc : ObjectStreamClass) : Class[_] = {
    // Check if the class was serialized.
    if (readBoolean) {
      Console.println("Special resolving class: " + desc.getName)
      val name = desc.getName
      val length = readInt
      Console.println("Length = " + length)
      val bytes = Array.ofDim[Byte](length)
      readFully(bytes)
      val loader = new SingleClassLoader(getClass.getClassLoader, name, bytes)
      loader.loadClass(name)
    } else super.resolveClass(desc)
  }
}

@serializable
class SerializeHelper[A <: AnyRef](private var inner : A) {
  // Retrieves the wrapped object.
  def get = inner

  @throws(classOf[IOException])
  private def writeObject(out: ObjectOutputStream): Unit = {
    val tmp = new InterpreterObjectOutputStream(out)
    tmp.writeObject(inner)
  }
  @throws(classOf[IOException])
  private def readObject(in: ObjectInputStream): Unit = {
    val tmp = new InterpreterObjectInputStream(in)
    inner = tmp.readObject.asInstanceOf[A]
  }
}
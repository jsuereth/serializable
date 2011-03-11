package serialize

import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.io.VirtualDirectory
import java.io.{ObjectStreamClass, ObjectInputStream, ObjectOutputStream, IOException}

class InterpreterObjectOutputStream(delegate : ObjectOutputStream) extends ObjectOutputStream(delegate) {
  lazy val classLoaderClass = classOf[AbstractFileClassLoader]
  private def isReplClassLoader(cl : ClassLoader) = {
    if (cl == null || cl.getClass == null) false
    else if (cl.getClass.getName == "scala.tools.nsc.interpreter.AbstractFileClassLoader") true
    else classLoaderClass.isAssignableFrom(cl.getClass)
  }
  @throws(classOf[IOException])
  override protected def annotateClass(cl : Class[_]) : Unit = {
    val extramagic = isReplClassLoader(cl.getClassLoader)
    // Mark whether we had to serialize the class.
    writeBoolean(extramagic)
    if (extramagic) {
      val cll = cl.getClassLoader.asInstanceOf[AbstractFileClassLoader]

      // TODO(joshuasuereth): Find a less hacky solution.
      val bytes = {
        val tmp = cll.findBytesForClassName(cl.getName)
        if (tmp.length == 0) {
          val tmp = cll.getClass.getDeclaredField("root")
          tmp.setAccessible(true)
          val dir = tmp.get(cll).asInstanceOf[VirtualDirectory]
          val fileName = cl.getName + ".class"
          val result = dir.iterator.find(_.name == fileName)
          Console.println("Found result: " + result)
          result.get.toByteArray
        } else tmp
      }
      Console.println("Serializing " + cl.getName + " with size = " + bytes.length + " from cl " + cll)
      if (bytes.length == 0) throw new IOException("Could not find REPL class for: " + cl.getName)
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
  } else null
  @throws(classOf[ClassNotFoundException])
  override def loadClass(name : String) : Class[_] = {
    var cl = findLoadedClass(name)
    if (cl == null) cl = findClass(name)
    if (cl == null) cl = parent.loadClass(name)
    cl
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
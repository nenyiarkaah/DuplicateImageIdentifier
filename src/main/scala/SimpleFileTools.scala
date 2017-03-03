import java.io._
import java.nio.file.Paths
import scala.io.Source
import scala.util.{Failure, Success, Try}
import java.nio.file.{Files, Paths, StandardCopyOption}

/**
  * Created by Nenyi on 23/02/2017.
  */
class SimpleFileTools extends SimpleJson {
  def codeSource = getClass.getProtectionDomain().getCodeSource()

  def jarFile = new File(codeSource.getLocation().toURI().getPath())

  def jarDir = jarFile.getParentFile().getPath()

  def getCurrentDirectory = new File(jarDir).toString

  def getSeparator = File.separator

  def getFiles(dir: String, extensions: Array[String]): Array[String] = {
    val casts = extensions.flatMap(ext => new File(dir).listFiles.filter(_.getName.endsWith(ext)))
    casts.map(c => c.getAbsolutePath)
  }

  def getFileName(path: String): String = {
    Paths.get(path).getFileName.toString
  }

  def containsSubFolders(dir: String): List[String] = {
    new File(dir).listFiles.filter(_.isDirectory).map(_.getAbsolutePath).toList
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def CreateFile(filename: String, settings: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(settings)
    bw.close()
  }

  def OpenAndReadFile(filename: String): String = {
    val stream = new FileInputStream(filename)
    ReadFile(stream) match {
      case Success(fileString) => fileString
      case Failure(f) => throw (f)
    }
  }

  def ReadFile(stream: FileInputStream): Try[String] = {
    Try(Source.fromInputStream(stream).mkString)
  }

  def checkIfFileExists(filename: String): Boolean = {
    Files.exists(Paths.get(filename))
  }
}

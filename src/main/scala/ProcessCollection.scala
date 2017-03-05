import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import java.util.Calendar

/**
  * Created by Nenyi on 28/02/2017.
  */
trait ProcessCollection extends ImageManipulation {

  val s = new SimpleFileTools
  val filename = s.getCurrentDirectory + s.getSeparator + "settings.json"

  def process = {
    val now = Calendar.getInstance().getTime()
    println(now)
    println(filename)

    def content = Try {
      s OpenAndReadFile (s createSettingJson(s checkIfFileExists (filename), filename))
    }

    content match {
      case Success(c) => val settings = s ConvertJsonToSettings (c)
        processFolderSequentially(settings.source, settings.extensions)
      case Failure(f) => println(f)
    }
  }

  def processFolderSequentially(dir: String, ext: Array[String]): Unit = {
    s containsSubFolders (dir) match {
      case c if c.length > 0 =>
        processSubFolder(dir, ext)
        c.map(processFolderSequentially(_, ext))
      case _ => println(dir)
        processSubFolder(dir, ext)
    }
  }

  def processSubFolder(dir: String, ext: Array[String]): Unit = {
    println("Scanning " + dir)
    val files = s getFiles(dir, ext)
    println(files.length + " files will be analysed")

    val info = analyseFiles(files)
    println(info.length + " files have been analysed")

    val dupes = identifyDuplicateHashValues(info)
    println(dupes.length + " duplicates have been found via Hash Value")
    val newInfo = removeImageInfoDuplicates(info, dupes)
    println("deleting " + dupes.length + " duplicates")
    //    deleteDuplicates(dupes)
    println("There are now " + newInfo.length + " to be analysed")

    val results = compareCombinations(newInfo)


    results.filter(_ != null).foreach(r => {
      println(r.a.path + " matches " + r.b.path)
      println("a:- " + r.a.name + " b:- " + r.b.name + " correlation " + r.correlation + " comparison " + r.comparison)
    })

    println("matches are " + results.length)

    val dupes2 = getCombinationDuplicates

    println("deleting " + dupes2.length + " duplicates")
    //    deleteDuplicates(dupes2)

    //    removeImageInfoDuplicates(newInfo, dupes2)
  }

  def processImagePair(pathA: String, pathB: String): Unit = {
    analyseImage(pathA) match {
      case None => println("unable to retrieve " + pathA)
      case Some(a) =>
        analyseImage(pathB) match {
          case None => println("unable to retrieve " + pathA)
          case Some(b) =>
            println(compareImages(a, b))
            println(compareImagesValue(a, b))
        }
    }

  }

}

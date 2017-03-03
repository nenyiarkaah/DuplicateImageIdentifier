/**
  * Created by Nenyi on 19/02/2017.
  */

import java.io._

import com.sksamuel.scrimage.ScaleMethod.{Bicubic, Bilinear, FastScale}
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.filter._
import org.github.jamm.MemoryMeter

import scala.collection.immutable.Stream.Empty
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ParSeq
import scala.util.{Failure, Success, Try}


class ImageManipulation {
  private val s = new SimpleFileTools
  private val threshold = 67.22
  private var compareIgnoreList = ListBuffer.empty[ImageInfo]

  private def loadImage(path: String): Image = {
    Try {
      Image.fromStream(new BufferedInputStream(new FileInputStream(path)))
    } match {
      case Success(img) => img
      case Failure(message) => println("Error loading image " + path)
        println(message)
        null
    }
  }

  def convertPixelsAndAddToSet(pixels: List[Pixel]): List[Int] = {
    pixels.map(p => p.hashCode)
  }

  def encryptPixels(pixels: Array[Pixel]): Array[Byte] = {
    Gzip.compress(pixels.map(_.argb).mkString(",").getBytes("UTF-8"))
  }

  def decryptPixels(bytes: Array[Byte]) = {
    Gzip.decompress(bytes).get.mkString("").split(",").toList
  }

  private def analyseImage(image: Image, path: String): ImageInfo = {
    System.gc()
    Try {
      image
        //      .filter(BlurFilter)
        .filter(GrayscaleFilter)
        .scaleTo(64, 64, Bilinear)
      //      .max(64,64)
      //        .scaleTo(8,16, FastScale)

    } match {
      case Failure(message) => println("Error Resizing " + path)
        println(message)
        null
      case Success(img) =>
        new ImageInfo(s getFileName (path), path, new Dimensions(image.dimensions._1, image.dimensions._2),
          image.hashCode(), encryptPixels(img.pixels))
    }

  }

  def analyseImage(path: String): ImageInfo = {
    analyseImage(loadImage(path), path)
  }

  def compareImages(a: ImageInfo, b: ImageInfo): ComparisonInfo = {
    val correlation = compareImagesValue(a, b)
    new ComparisonInfo(a, b, correlation, correlation >= threshold)
  }

  def compareImagesValue(a: ImageInfo, b: ImageInfo): Double = {
    val aPixels = decryptPixels(a.pixels)
    val bPixels = decryptPixels(b.pixels)
    (aPixels zip bPixels filter (x => x._1 == x._2) size).toDouble / aPixels.length.toDouble * 100
  }

  def compare(a: ImageInfo, b: ImageInfo): ComparisonInfo = {
    if (compareIgnoreList.exists(_ == a) || compareIgnoreList.exists(_ == b)) null
    else {
      System.gc()
      val comparison = compareImages(a, b)
      comparison.comparison match {
        case false => null
        case true =>
          compareIgnoreList = compareIgnoreList += identifyLowResDuplicate(a, b)
          comparison
      }
    }
  }

  def identifyDuplicateHashValues(info: Stream[ImageInfo]) = {
    val dupes = info.filter(i1 =>
      info.groupBy(i => i.hashValue).collect { case (x, ys) if ys.size > 1 => x }.exists(_ == i1.hashValue))
      .groupBy(_.hashValue)

    dupes.foreach(d => println(d._2.map(_.name).mkString(", ") + " contains HashValue " + d._1))

    generateDuplicateHashValueRemoveList(dupes)
  }

  private def identifyLowResDuplicate(a: ImageInfo, b: ImageInfo): ImageInfo = if (a.dimensions < b.dimensions) a else b

  private def generateDuplicateHashValueRemoveList(dupes: Map[Int, Stream[ImageInfo]]) = {
    dupes.map(_._2.sortBy(_.dimensions.height).reverse.tail).toList.flatten
  }

  def removeImageInfoDuplicates(infolist: Stream[ImageInfo], dupes: List[ImageInfo]) = infolist.filterNot(dupes.toSet)

  def deleteDuplicate(d: ImageInfo) = {
    val file = new File(d.path)
    if (file.exists()) file.delete
  }

  def deleteDuplicates(dupilcates: List[ImageInfo]) = {
    dupilcates.par.foreach(d =>
      deleteDuplicate(d)
    )
  }

  def getCombinationDuplicates() = compareIgnoreList.toList

  def analyseFiles(files: Array[String]) = files.par.map(f => analyseImage(f)).filter(_ != null).toStream

  def compareCombinations(infoList: Stream[ImageInfo]): List[ComparisonInfo] = {

    val combinations = infoList.combinations(2).toStream
    traverseCombinations(combinations, Nil)
  }

  def traverseCombinations(stream: Stream[Stream[ImageInfo]], compareInfo: List[ComparisonInfo]): List[ComparisonInfo] = {
    stream match {
      case Stream.Empty => compareInfo
      case pair #:: tail =>
        logMemoryUsage(stream, "combinations")
        logMemoryUsage(compareInfo, "compareInfo")
        compare(pair.head, pair.last) match {
          case null => traverseCombinations(tail.filter(_ != pair), compareInfo)
          case default => traverseCombinations(tail.filter(_ != pair), compareInfo :+ default)
        }
    }
  }

  def logMemoryUsage(o: Object, name: String): Unit = {
    val mm = new MemoryMeter()
    println("Size of " + name + ": " + humanReadableConversion(mm.measureDeep(o), true))
  }

  def humanReadableConversion(bytes: Long, si: Boolean) = {
    val sizes = Array("B", "KB", "MB", "GB", "TB")
    var order = 0
    var length = bytes
    while (bytes >= 1024 && order < sizes.length - 1) {
      order += 1
      length = length / 1024
    }
    length.toString + sizes(order)
  }

}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.io.BufferedSource
import scala.util.Try

/**
  * Created by Nenyi on 28/02/2017.
  */
object Gzip {
  def compress(input: Array[Byte]): Array[Byte] = {
    val bos = new ByteArrayOutputStream(input.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(input)
    gzip.close()
    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  def decompress(compressed: Array[Byte]): Option[BufferedSource] =
    Try {
      val inputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
      scala.io.Source.fromInputStream(inputStream)
    }.toOption
}

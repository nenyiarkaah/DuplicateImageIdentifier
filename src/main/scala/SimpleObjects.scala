/**
  * Created by Nenyi on 01/03/2017.
  */
case class Settings(source: String, extensions: Array[String])

case class Dimensions(height: Int, width: Int) {

  def canEqual(a: Any) = a.isInstanceOf[Dimensions]

  override def equals(that: Any): Boolean = that match {
    case that: Dimensions => that.canEqual(this) && this.hashCode == that.hashCode
    case _ => false
  }

  override def hashCode: Int = {
    return height + width
  }

  def <(that: Any): Boolean = that match {
    case that: Dimensions => that.height < this.height && that.width < this.width
    case _ => false
  }
}

case class ImageInfo(name: String, path: String, dimensions: Dimensions, hashValue: Int, pixels: Array[Byte])

case class ComparisonInfo(a: ImageInfo, b: ImageInfo, correlation: Double, comparison: Boolean)
/**
  * Created by Nenyi on 01/03/2017.
  */

import SettingsJsonProtocol._
import spray.json._

trait SimpleJson {
  def createSettingJson(doesExist: Boolean, filename: String): String = {

    doesExist match {
      case false =>
        val simpleFile = new SimpleFileTools
        val settings = new Settings(simpleFile.getCurrentDirectory + simpleFile.getSeparator + "Images",
          Array("jpg", "png", "jpeg")).toJson
        simpleFile.CreateFile(filename, settings.toString)
        throw new NoSuchElementException("Settings file has just been created, please configure the source and destination paths")
      case _ => filename
    }
  }


  def ConvertJsonToSettings(content: String): Settings = {
    content.parseJson.convertTo[Settings]
  }
}

import spray.json.DefaultJsonProtocol

/**
  * Created by Nenyi on 01/03/2017.
  */
object SettingsJsonProtocol extends DefaultJsonProtocol {
  implicit val settingsFormat = jsonFormat2(Settings)
}

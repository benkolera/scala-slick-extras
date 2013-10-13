import org.specs2.mutable._

class ImplicitsSpec extends Specification {

  "The Implicits example from the readme" should {
    "Compile :)" in {
      import org.joda.time.LocalDate
      import com.benkolera.slick.Implicits._
      import scala.slick.jdbc.{StaticQuery => Q}
      import Q.interpolation

      case class Person( id:Int , name:String, dob:LocalDate )

      implicit val personGetResult = getResultCaseClass3( Person.apply _ )
      val peopleQuery = sql"""SELECT id,name,dob FROM person""".as[Person]

      true must_==( true )

    }
  }

}

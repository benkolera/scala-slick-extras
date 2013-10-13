scala-slick-extras
============

Handy collection of GetResult / Set Parameter instances for common types (plus helper for case classes)

```scala
  import org.joda.time.LocalDate
  import com.benkolera.slick.Implicits._
  import scala.slick.jdbc.{StaticQuery => Q}
  import Q.interpolation

  case class Person( id:Int , name:String, dob:LocalDate )

  implicit val personGetResult = getResultCaseClass3( Person.apply _ )
  val people = sql"""SELECT id,name,dob FROM person""".as[Person].list
```

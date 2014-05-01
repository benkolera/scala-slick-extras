scala-slick-extras
============

Handy collection of GetResult / Set Parameter instances for common types (plus helper for case classes). Very tailored towards my projects that are usually some combination of argonaut and postgres, so you may just want to steal from this rather than incurring a dependency on the postgres driver or argonaut. 

```scala
  import org.joda.time.LocalDate
  import com.benkolera.slick.Implicits._
  import scala.slick.jdbc.{StaticQuery => Q}
  import Q.interpolation

  case class Person( id:Int , name:String, dob:LocalDate )

  implicit val personGetResult = getResultCaseClass3( Person.apply _ )
  val people = sql"""SELECT id,name,dob FROM person""".as[Person].list
```

Or if you're using the PG json datatype in a less-sql fashion.

```scala
  import argonaut._, Argonaut._
  import org.joda.time.LocalDate
  import com.benkolera.slick.Implicits._
  import com.benkolera.slick.Argonaut._
  import scala.slick.jdbc.{StaticQuery => Q}
  import Q.interpolation
  
  case class Person( id:Int , json:PersonJson )
  case class PersonJson( name:String, dob:LocalDate )

  implicit val personCodec = casecodec2( PersonJson.apply , PersonJson.unapply )(
    "name","dob"
  )
  implicit val personGetResult = getResultCaseClass3( Person.apply _ )
  val people = sql"""SELECT id,name,dob FROM person""".as[Person].list  
```

Changes to com.benkolera scala-slick-extras
===========================================

2013-10-13 - 0.2.0 - Ben Kolera
- Added PgLocalDateTime to handle Pgs infinite timestamps.
- Cleaned up code for Getters/Setters
- Made LocalDateTime sqlToLdt error hinting at PgLocalDateTime when it sees infinity.

2013-10-13 - 0.1.0 - Ben Kolera
-------------------------------
- First release!
- GetResult helpers for case class constructors
- GetResult/Set Parameter instances for 
  - org.joda.time.{LocalDateTime,LocalDate,LocalTime} 
  - java.nio.file.Path	

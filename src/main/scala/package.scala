package com.benkolera

import scalaz._
import scala.slick.jdbc.JdbcBackend.Session

package object slick {
  /*
   * Because I really don't like how JDBC and Session.withTransaction works:
   * - Autocommit should be true after the transaction just like begin & commit.
   * - I hate how it forces me to throw exceptions to roll back.
   */
  def withTransaction[E,A]( session:Session )( f: => E \/ A ): E \/ A  = {
    try {
      session.withTransaction{
        f.leftMap( e => { session.rollback ; e } )
      }
    } finally {
      session.conn.setAutoCommit(true)
    }
  }

}

package dirsplit

import org.scalatest._
import scala.io.Source

class DirsplitSpec extends FlatSpec with Matchers {

  // val source = Source.fromURL(getClass.getResource("/splitdir-test"))

  // filesToMove()
  // –––

  "A file or dir" should "not be moved if it starts with a dot" in {
    val target = new java.io.File("/tmp/splitdir-test").listFiles
    val files = Dirsplit.filesToMove(target).map(_.getName)
    files should not contain ".hello"
    files should not contain ".dot-dir"
    files should not contain ".README.md"
  }

  it should "not be moved if the directory name has a YYYY-MM scheme" in {
    val target = new java.io.File("/tmp/splitdir-test").listFiles
    val files = Dirsplit.filesToMove(target).map(_.getName)
    files should not contain "2015-09"
  }

}

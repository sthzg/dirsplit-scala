package dirsplit

import java.io.File
import java.nio.file.attribute.FileTime
import java.nio.file._
import java.time._
import java.time.format.DateTimeFormatter


object Dirsplit {

  def main(args: Array[String]) = {
    val files = new File(".").listFiles
    val fileList = filesToMove(files)
    val moveMap = buildMoveMap(fileList)
    printReportOnStdOut(moveMap)
//    moveFiles(moveMap)
  }

  def filesToMove(files: Array[File]): Array[File] = {
    files.filterNot(file => isDotFile(file) || isMonthDir(file))
  }

  def buildMoveMap(files: Array[File]):Seq[(String, File)] = {
    files.map(file => {
      val fileTime: FileTime = Files.getLastModifiedTime(Paths.get(file.getAbsolutePath))
      val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM").withZone(ZoneId.systemDefault())
      (formatter.format(fileTime.toInstant), file)
    })
  }

  def moveFiles(moveMap: Seq[(String, File)]) = {
    for (item <- groupFilesByMonth(moveMap)) {
      val dirName = item._1
      println(s"${Console.YELLOW}Moving ${item._2.size} files to ${item._1}")
      for (data <- item._2) {
        val file: File = data._2
        val source: Path = Paths.get(file.getAbsolutePath)
        val target: Path = Paths.get(file.getParent + "/" + dirName + "/" + source.getFileName)
        if (!Files.exists(target.getParent, LinkOption.NOFOLLOW_LINKS)) {
          Files.createDirectory(target.getParent)
        }
        try {
          Files.move(source, target, StandardCopyOption.ATOMIC_MOVE)
          println(s"  ${Console.YELLOW}Moved ${Console.BLACK}${source.getFileName} ${Console.YELLOW}to ${Console.BLACK}$target")
        } catch {
          case e: AccessDeniedException => println(s"  ${Console.RED}Error moving ${source.getFileName}. Access denied")
        }
      }
    }
  }

  def printReportOnStdOut(moveMap: Seq[(String, File)]) = {
    for (item <- groupFilesByMonth(moveMap)) {
      println(s"${Console.YELLOW}${item._1} ––– ${item._2.size} files")
      for (data <- item._2) {
        val file = data._2
        val fileOrDir = if (file.isFile) s"${Console.GREEN}FILE\t" else s"${Console.GREEN}DIR\t"
        println(s"  $fileOrDir${Console.BLUE}${file.getName}")
      }
    }
  }

  private[this] def groupFilesByMonth(moveMap: Seq[(String, File)]): Seq[(String, Seq[(String, File)])] = {
    moveMap.groupBy(_._1).toSeq.sortBy(_._1)
  }

  private[this] def isMonthDir(dir: File): Boolean = {
    if (!dir.isDirectory) false
    val pattern = "^\\d{4}-\\d{2}$".r
    pattern findFirstIn dir.getName match {
      case None => false
      case _    => true
    }
  }

  private[this] def isDotFile(file: File): Boolean = {
    file.getName.startsWith(".")
  }

}

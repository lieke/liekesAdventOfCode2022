import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

var testInput = List("ls","dir a","14848514 b.txt","8504156 c.dat","dir d","cd a","ls","dir e","29116 f","2557 g","62596 h.lst","cd e","ls","584 i","cd ..","cd ..","cd d","ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k")

class ExampleSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  var directoryStructure = new DirectoryStructure()
  before {
    directoryStructure = new DirectoryStructure(testInput)
  }

  "A single file" should "be properly processed" in {
    val newFile = directoryStructure.allFiles.get("b.txt").get
    newFile.size should be (14848514)
  }

 "A single directory" should "be properly processed" in {
    val newDir = directoryStructure.allFiles.get("a").get
    newDir.size should be (0)
  }

  "A file" should "be added to a directory" in {
    directoryStructure.allFiles.get("ae").get.children(0).size should be (584)
  }

  "All the files" should "be added to their own directory" in {
    directoryStructure.allFiles.get("/").get.children.length should be (4)
    directoryStructure.allFiles.get("a").get.children.length should be (4)
    directoryStructure.allFiles.get("d").get.children.length should be (4)
    directoryStructure.allFiles.get("d").get.getChildren() should be ("List(k, d.ext, d.log, j)")
    directoryStructure.allFiles.get("ae").get.children.length should be (1)
    directoryStructure.allFiles.get("ae").get.getChildren() should be ("List(i)")
  }

  "The size of the directory" should "be calculated" in {
    directoryStructure.allFiles.get("ae").get.getSize() should be (584)
    directoryStructure.allFiles.get("d").get.getSize() should be (24933642)
    directoryStructure.allFiles.get("a").get.getSize() should be (94853)
    directoryStructure.allFiles.get("/").get.getSize() should be (48381165)
  }

  "The size of all directories" should "be calculated" in {
    directoryStructure.findFilesWithAtMostCertainSize(100000000).sortWith(_ < _) should be (List(584, 94853, 24933642, 48381165))
    directoryStructure.findFilesWithAtMostCertainSize(100000).sortWith(_ < _) should be (List(584, 94853))
    val totalSizesOfSmallerDirectories = directoryStructure.findFilesWithAtMostCertainSize(100000).sum
    totalSizesOfSmallerDirectories should be (95437)
  }

  "The smallest directory that frees up enough space" should "be found" in {
    directoryStructure.findSizeOfSmallestDirectoryThatFreesUpEnoughSpace() should be (24933642)
  }
}


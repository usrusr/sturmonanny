package de.immaterialien.qlmap
import java.io._
import scala.xml._

class HtmlMissionFilter(args: String) extends javax.xml.ws.Provider[File] with Log {
  val mapBase = new MapBase(new File(args))
  val groundClasses = new GroundClasses(args)
  override def invoke(file: File): File = {

    val thread = new Thread("recon " + file.getName) {
      override def run {
        val parsed = new MisParser(file, mapBase, groundClasses)
        val img = MisRender.paint(file, parsed.out)
        //        val img = Some(new File("src/test/resources/Afrika_42194204060.mis.JPG"))
        for (i <- img) new HtmlUpdater(i).update
      }
    }
    thread.setPriority(Thread.MIN_PRIORITY)
    thread.start
    file
  }
}

object HtmlMissionFilter {
  val imageFiles = new FilenameFilter() {
    override def accept(path: File, fname: String): Boolean = fname.toLowerCase.endsWith(".mis.jpg.recon.html")
  }
}
class HtmlUpdater(path: File) {

  val parent = path.getParentFile
  val newImage = path.getName

  def writeListing {
    val images = parent.listFiles(HtmlMissionFilter.imageFiles)
    val sorted = images.sortBy(f => (-1L * f.lastModified))

    val misList = <html><body style="font-size:smaller;">
                          {
                            sorted.map { file =>
                              val fname = file.getName
                              val name = fname.substring(0, fname.indexOf("."))
                              //name = fname.substring(0, fname.indexOf("."))
                               <div><a href={ fname } target="_parent">{ name }</a><br/></div>
                            }
                          }
                        </body></html>
    XML.save(parent.getAbsolutePath + "/recon.list.html", misList)
  }
  def update {
    val name = newImage.substring(0, newImage.indexOf("."))
    val mis = <html>
                <head>
                  <title>{ name }</title>
                </head>
                <body>
                  <table>
                    <tr>
                      <td><img src={ newImage }/></td>
                      <td valign="top">
                        { name }<br/><div style="font-size:smallest;">Timeline:</div><iframe src="recon.list.html" width="150px" style="border:none;"/>
                      </td>
                    </tr>
                  </table>
                </body>
              </html>
    XML.save(parent.getAbsolutePath + "/" + newImage + ".recon.html", mis)
    XML.save(parent.getAbsolutePath + "/recon.latest.html", mis)
    writeListing
  }
}
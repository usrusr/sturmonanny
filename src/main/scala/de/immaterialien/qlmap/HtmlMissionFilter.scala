package de.immaterialien.qlmap
import java.io._
import scala.xml._
import de.immaterialien.sturmonanny.fbdjhosting._

class HtmlMissionFilter(args: String) extends javax.xml.ws.Provider[File] with Log with NonMutatingFilter  {
  val mapBase = new MapBase(new File(args))
  val groundClasses = new GroundClasses(mapBase.folder.getAbsolutePath )
  def afterDone = ()
  override def invoke(file: File): File = {

    val thread = new Thread("recon " + file.getName) {
      override def run {
      	inline(file)
      	afterDone
      }
    }
    thread.setPriority(Thread.MIN_PRIORITY)
    thread.start
    file
  }
  def inline(file: File){
     val parsed = new MisParser(file, mapBase, groundClasses)
     val mapWriter = new scala.collection.mutable.ArrayBuffer[Elem]
     val img = MisRender.paint(file, parsed.out, mapBase, Some(mapWriter))
     debug("created "+img.map(_ getAbsolutePath))
     for (i <- img) new HtmlUpdater(i, file, mapWriter).update

  }
  override def toString = "html enabled recon renderer at "+args + " ("+mapBase.folder.getAbsolutePath+")"
}

object HtmlMissionFilter {
  private val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd")
  def formatDate(timestamp:Long) = formatter synchronized {
    formatter.format(new java.util.Date(timestamp))
  }
  private val formatterTime = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  def formatDateTime(timestamp:Long) = formatterTime synchronized {
    formatterTime.format(new java.util.Date(timestamp))
  }
  val commonMissionPrefix = """^(\d*\D*)""".r
	class Inline(args: String) extends TalkingHtmlMissionFilter(args){
	  override def invoke(file: File): File = {
     	inline(file)
     	afterDone
     	file
    }
	}  
}
class HtmlUpdater(path: File, mission: File, mapContent:Seq[Elem]) {
  import HtmlMissionFilter._
  val misFolder = mission.getParentFile 
  val currentMisFilePrefix = commonMissionPrefix.findPrefixOf(mission.getName)
  val lowerPrefix = currentMisFilePrefix.getOrElse("").toLowerCase
  val misFilter = new FilenameFilter {
    override def accept(path: File, fname: String): Boolean = {
      val ln = fname.toLowerCase
      val ret = ln.endsWith(".mis") && ln.startsWith(lowerPrefix)
      ret
    }
  }

  /**
   * finding all .mis.recon.html within our current campaign
   */
  val campaignPrefix = {
    val files = misFolder.listFiles(misFilter)
    val oldest: File = files min Ordering[Long].on{f:File=>f.lastModified}
    currentMisFilePrefix.map(_ + ".").getOrElse("") + formatDate(oldest.lastModified) + "."
  }
  /**
   * finding all .mis.recon.html within our current campaign
   */
  val reconFilter = new FilenameFilter {
    val lowerCampaign = campaignPrefix.toLowerCase
    override def accept(path: File, fname: String): Boolean = {
      val ln = fname.toLowerCase
      val ret = ln.endsWith(".mis.recon.html") && ln.startsWith(lowerCampaign)
      ret
    }
  }  
  val parent = path.getParentFile
  val newImage = path.getName

  def writeListing {
    val recons = parent.listFiles(reconFilter)
    val sorted = recons.sortBy(f => (-1L * f.lastModified))

    val misList = <html>
<head><link rel="stylesheet" href="../customizerecon/styles.css" type="text/css"></link></head>
<body style="font-size:smaller;">
                          {
                            sorted.map { file =>
                              val fname = file.getName
                              val rawName = fname.substring(campaignPrefix.length)
                              val name = rawName.substring(0, rawName.indexOf("."))
                              //name = fname.substring(0, fname.indexOf("."))
                               <div><a href={ fname } target="_parent">{ name }</a><br/></div>
                            }
                          }
                        </body></html>
    XML.save(parent.getAbsolutePath + "/"+campaignPrefix+"list.recon.html", misList)
  }
  def update {
    val name = newImage.substring(0, newImage.indexOf("."))
    val mis = <html>
                <head>
                  <title>{ name }</title>
    							<link rel="stylesheet" href="../customizerecon/styles.css" type="text/css"></link>
                </head>
                <body>
                  <table>
                    <tr>
                      <td>{
    										if(mapContent.isEmpty) <img src={ newImage } />
    										else <img src={ newImage } usemap="#mouseover" />
                      }</td>
    											{if( ! mapContent.isEmpty){
    												<map name="mouseover">
    													{mapContent}
    												</map>
													}}
                      <td valign="top" height="100%">
                        { name }<br/>
                        <a href="latest.recon.html" title="jump to latest">{ formatDateTime(mission.lastModified) }</a><br/>
                        <div style="font-size:smallest;">Timeline:</div><iframe src={campaignPrefix+"list.recon.html"} width="150px" height="100%" style="border:none;"/>
                      </td>
                    </tr>
                  </table>
                  <p style="font-size:smallest;">{de.immaterialien.sturmonanny.core.Server.initVersion}</p>
                </body>
              </html>
    XML.save(parent.getAbsolutePath + "/"+campaignPrefix + name + ".mis.recon.html", mis)
    XML.save(parent.getAbsolutePath + "/"+campaignPrefix+"recon.html", mis)
    XML.save(parent.getAbsolutePath + "/latest.recon.html", mis)
    writeListing
  }
}


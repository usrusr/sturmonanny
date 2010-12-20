package de.immaterialien.qlmap

import java.io._
import scala.util.parsing.combinator._

class MapConfigurationImpl(val file: File) extends MapConfiguration with Log{
  val (
    outPath: Option[File]
    ) = {
    val reader = scala.io.Source.fromFile(file)
    var outPath: Option[File] = None

    object parser extends RegexParsers with Log {
      lazy val comment = opt("#" ~ ".*")
       
      lazy val keyValue = 
      		"\\S+".r ~ "=" ~ (("\"" ~> """(\S(:?.*\S)?""".r <~ "\"") | """([^"\\]|(\\[^\\])|(\\\\))*""".r) <~ comment ^^ {
        case key ~ _ ~ value => {
          key match {
            case "reconDir" => { val outPathString = value
              log.debug("reconDir:" + outPathString)
              val out = if (outPathString startsWith ".") new File(file.getParent + File.separator + outPathString)
              else new File(outPathString)
    
              if (out.isFile) MapConfigurationImpl.this.log warn(out.getAbsolutePath + " is a file, need a directory")
              else if (out.exists || out.mkdirs) {
                outPath = Some(out)
              } else MapConfigurationImpl.this.log warn("failed to create recon directory " + out.getAbsolutePath)
            }
            case x => MapConfigurationImpl.this.log warn("unkown key " + x) 
          }
        }
      }
      
      lazy val all = keyValue | comment
    }

    for(line<-reader.getLines) parser.parseAll(parser.all, line)

    reader.close
    (outPath)
  }
}
trait MapConfiguration {
	def outPath:Option[File]
}
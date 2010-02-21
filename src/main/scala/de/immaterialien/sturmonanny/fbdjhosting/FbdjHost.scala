package de.immaterialien.sturmonanny.fbdjhosting

import org.mortbay.jetty.webapp
import de.immaterialien.sturmonanny.core.UpdatingMember
import de.immaterialien.sturmonanny.util.Logging


class FbdjHost(val jarPath : String, overridesPath : String, configurationPath:String)  extends Logging {
		
		val jarFile = new java.io.File(jarPath)
  	val jarUrl = jarFile.toURL
   
    
  	val overrideFile = new java.io.File(overridesPath)
  	val overrideUrl = overrideFile.toURL

   	if( ! jarFile.canRead || ! overrideFile.canRead ){
   	  var list : List[String] = Nil
   	  if( ! jarFile.exists){
   	    list ::= "FBDj jar '"+jarFile.getAbsoluteFile+"' does not exist"
   	  }else if( ! jarFile.canRead){
   	    list ::= "FBDj jar '"+jarFile.getAbsoluteFile+"' cannot be read"
   	  } 
   	  if( ! overrideFile.exists){
   	    list ::= "FBDj overrides '"+overrideFile.getAbsoluteFile+"' do not exist"
   	  }else if( ! overrideFile.canRead){
   	    list ::= "FBDj overrides '"+overrideFile.getAbsoluteFile+"' cannot be read"
   	  } 
      
      if( ! list.isEmpty) throw new java.io.FileNotFoundException(list.mkString(" and ")+", please check your configuration") 
   	}
   

  	val parent = this.getClass.getClassLoader
   
  	try{
  		parent.loadClass("main.FBDj")
  		throw new IllegalStateException("Error starting FBDj container: FBDj.jar must not be on the classpath, it should only be referenced from the sturmonanny configuration!")
    }catch{
      case _:ClassNotFoundException => // check passed, no FBDj.jar on classpath
    }
  	val classLoader = new java.net.URLClassLoader(Array(overrideUrl, jarUrl), parent)
  	val mainClass = try{
  		classLoader.loadClass("main.FBDj")
    }catch{
      case c:ClassNotFoundException => throw new ClassNotFoundException("Could not load FBDj.jar from "+jarUrl+" ")
    }
		val mainMethod = mainClass.getMethod("main", classOf[Array[String]])
  
		val thread = new Thread {
		  override def run = {
				  val parameters :Array[Array[String]]= Array(Array("config="+configurationPath))
debug("starting FBDj with "+parameters(0).mkString)      
				  mainMethod.invoke(null, parameters)
	//			  mainMethod.invoke(null, Array(args):_* )
      }
		}
		thread run
    
    def stop = {
      if(thread!=null) thread interrupt
    }
   
// discarded idea: 
//   use a classloader to map from some.package.SomeFbdjClass 
//   to de.immaterialien.sturmonanny.fbdjhosting.override.some.package.SomeFbdjClass
// this could maybe even work, as long as the overrides extend the original?
// 		   
//  	object classLoader extends java.net.URLClassLoader(Array(jarUrl), parent){
//			val overridePrefix = this.getClass.getPackage.getName + ".override."
//  	 /**
//      * core of our custom loading: 
//  	  */
//  	  def select(name:String) : java.lang.Class[_] = {
//  	    try{
//  	      val actualName = overridePrefix+name
//  	    	val overridden = parent.loadClass(actualName)
//  	    	
//  	    	overridden
//        }catch{
//          case _ => {
//            
//            try{
//            	val local = this.findClass(name)
//            	local
//            }catch{
//              case _ => parent.loadClass(name)
//            }
//          }
//        }
//  	  }
//  	  
////			override def findClass(name:String) = {
////			  parent
////			}
//			override def loadClass(name:String, resolve:Boolean) : java.lang.Class[_] ={
//			  val existing = findLoadedClass(name)
//			  if(existing!=null){
//			    existing
//			  }else{
//			    select(name)
//			  }
//			}
//  	}
  
                                                                                                        
//		val jarUrl = jarFile.
//  	var classLoader : ClassLoader = null 
//  extends java.net.URLClassLoader(){
//		  
//		} 
  
  
//	val webconf = new webapp.JettyWebXmlConfiguration() 
//	webconf.configureDefaults
// 
// /*    */ public class FBDj
///*    */ {
///*    */   public static void exitProgram()
///*    */   {
///*    */   }
///*    */ 
///*    */   public static void main(String[] args)
///*    */   {
///* 24 */     MainController.startupConfigInitialize(args);
///*    */ 
///* 27 */     MainController.configInitialize();
///* 28 */     MainController.writeDebugLogFile(1, "*******************************************************************");
///* 29 */     MainController.writeDebugLogFile(1, "*******************************************************************");
///*    */ 
///* 31 */     MainController.writeDebugLogFile(1, "FBDj Started with Configuration ( " + MainController.CONFIG.getConfigName() + " )");
///*    */ 
///* 34 */     IL2DataLoadController.loadAllDataFiles();
///*    */ 
///* 37 */     MainController.setAdmins(AdminController.adminsLoad());
///*    */ 
///* 40 */     MainController.setReservedNames(ReservedNameController.reservedNamesLoad());
///*    */ 
///* 43 */     MainController.setBannedPilots(PilotBanController.pilotBansLoad());
///*    */ 
///* 45 */     MainWindowApp2.main(null);
///*    */   }
///*    */ }
	
}

//package de.immaterialien.sturmonanny.core
//
//import _root_.de.immaterialien.sturmonanny.util._
//import _root_.de.immaterialien.sturmonanny.util.event
//import _root_.de.immaterialien.sturmonanny.util.configgy.ConfigurationSchema
//
//import java.sql.SQLException
//import java.sql.Timestamp
////import org.squeryl.tests.QueryTester
////import _root_.org.squeryl.Session
////import _root_.org.squeryl.adapters.H2Adapter
// 
//import _root_.org.squeryl._  
//import adapters._
//import org.squeryl.PrimitiveTypeMode._
//import dsl.ast.BinaryOperatorNodeLogicalBoolean
//import dsl.{EnumExpression, StringExpression, Measures, GroupWithMeasures}
//import java.util.{Date, Calendar}
//
//class SquerylContext extends Logging with UpdatingMember{ import SqueryContext._
//	def apply(body: (()=>_) ){
//		val jdbc=server.conf.persistence.jdbc
//		val session = Session.create(java.sql.DriverManager.getConnection(jdbc.url.apply, jdbc.user.apply, jdbc.pass.apply), new H2Adapter)
//    SessionFactory.concreteFactory = Some (() => session)
//    try{
//    	using(session){
//      transaction{ 
//      	body()
//      }
//    	}
//    }catch{
//    	case e => error("",e)
//    }
//    session.close
//	}
////	var url=""
//	//java.sql.DriverManager.getConnection("jdbc:h2:/tmp/squeryltst;AUTO_SERVER=true"),new H2Adapter) 
//	
//	def updateConfiguration{
//		val jdbc=server.conf.persistence.jdbc
//		try{
//			try{
//				Class.forName(jdbc.driver.apply)
//			}catch{
//				case e=> {error("could not load "+jdbc.driver.apply, e); throw e}
//			}
////			url=jdbc.url.apply
////			url=jdbc.url.apply
////			url=jdbc.url.apply
//			
//			
//		}catch{
//			case e=> error("failed to init db")
//		}
//	}
//}
//object SqueryContext {
//	//new Session(customerId2DatabaseUrlMap.get(CustomerIdHolder.id), new YourDBAdapter) 
//}
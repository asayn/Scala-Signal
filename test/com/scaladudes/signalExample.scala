package com.scaladudes

object signalExample extends Application {

  import com.scaladudes.signal._
  

  object objA extends SignalEmitter {
  	case class MySignal(s:String, i:Int) extends Signal;
  
  	def doSomething(s:String, i:Int) = {
      // triggers MySignal with the values passed in parameters
      emit(MySignal(s,i))
    }
  }

  object objB {
    def handleString(s:String) = {println("objB.handleString("+s+")")}
  }

  object objC {
    def handleInt(i:Int) = {println("objC.handleInt("+i+")")}
  }
  
  val c1 = connect[objA.MySignal] {case objA.MySignal(s,_) => objB.handleString(s)} 
  val c2 = connect[objA.MySignal] {case objA.MySignal(_,i) => objC.handleInt(i)}
  
  objA.doSomething("abcd", 7)
  
  disconnect(c1)

  objA.doSomething("efgh", 12)
  
  stopDispatcher
  
}
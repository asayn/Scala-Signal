package com.scaladudes

object signalExample extends Application {

  import com.scaladudes.signal._
  
  /**
   * Generic class that wraps a variable and emits the "ContentChanged" signal
   * as soon as the variable's value has changed   
   */
  class Cell[T](var value:T) extends SignalEmitter {
    case class ContentChanged(value:T) extends Signal
    
    def setValue(newValue:T) = {
      if (value!=newValue) {
        value = newValue
        emit(ContentChanged(value))
      }
    }
  }
  
  // a Cell wrapping an Int 
  val intCell = new Cell[Int](1);
  
  // another Cell wrapping a String
  val stringCell = new Cell[String]("not set yet");
  
  // connects intCell to stringCell
  connect[intCell.ContentChanged] {case intCell.ContentChanged(i) => stringCell.setValue("set to "+i)}

  // connects intCell in order to print its content
  connect[intCell.ContentChanged] {case intCell.ContentChanged(i) => println("intCell's value changed to "+i)}

  // prints the stringCell content when it changes
  connect[stringCell.ContentChanged] {case stringCell.ContentChanged(s) => println("stringCell's value changed to \"" + s + "\"")}
  
  // set intCell 
  intCell.setValue(5) // it should produce the following output:
                      //    | intCell's value changed to 5
                      //    | stringCell's value changed to "set to 5"
  
}
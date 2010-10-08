/**
* Copyright (C) 2009-2010 Anthony Sayn <http://scaladudes.com>
*/
package com.scaladudes

/**
 * Singleton providing all the Signal-related facilities.<br>
 * This object delivers the key entities: the <code>Signal</code> class, the 
 * <code>SignalEmitter</code> trait, and the <code>connect</code>, <code>disconnect</code>
 * & <code>emitSignal</code> methods.<br>
 * It stores all the active connections. It controls the dispatcher Actor responsible
 * for notifying (asynchronously) all the entities connected to a given <code>Signal</code> 
 * when it gets emitted.
 * 
 * @author <a href="http://scaladudes.com">Anthony Sayn</a>
 */
object signal {
  import scala.actors.Actor
  
  /**
   * Stores all the active connections. Each <code>Signal</code> is associated
   * with its connections by mapping its <code>scala.Predef.Manifest</code> to
   * a <code>List</code> of <code>PartialFunction</code>s 
   */
  private var connectionMap = Map.empty[Any,List[PartialFunction[_, _]]]
  
  /**
   * Allows to stop the dispatcher
   */
  private case class Stop
  
  /**
   * Dispatcher Actor. It receives <code>(manifestName:String, signal:Signal)</code>
   * tuples and sends the first item <code>signal</code> to all the functions connected
   * to the <code>Signal</code> which Manifest name is <code>manifestName<code>
   */
  private val dispatcher = new Actor {
    def act() {
      var stop = false
      while (!stop) {
        receive {
          case (manifestName, signal) => {
            try {
              // retrieves the connections
              connectionMap(manifestName) foreach { c =>
                try {
                  // invoke the connected partial function with the passed-in signal
                  c.asInstanceOf[PartialFunction[Any,_]](signal)
                }
                catch {
                  case _ => //nothing to do
                }
              }
              true
            }
            catch {
              case _ => false // can't emit the signal
            }
          }
          case Stop => stop=true
        }
      }
    }
  }

  // starts the dispatcher as soon as required
  dispatcher.start()

  
  /**
   * "Secret owner" of a <code>Signal</code> case class. This abstraction allows to
   * block the invocation of the <code>emit</code> method from outside the entity owning 
   * the <code>Signal</code>.   
   */
  protected trait SignalOwner
  
  /**
   * Signal abstraction. Any signal must extend this class.
   * An instance of <code>SignalOwner</code> is required. This "Secret owner" instance is
   * implicitly provided by the <code>SignalEmitter</code> trait.
   */
  class Signal(implicit owner:SignalOwner) {
    /**
     * provides the "Secret owner". Only accessible inside the <code>signal</code> singleton
     */
    private[signal] def getOwner() = owner
  }
  
  /**
   * Signal emitter abstraction. Any entity that wants to declare <code>Signal</code>s has to 
   * implement this trait. 
   */
  trait SignalEmitter {
    /**
     * "Secret owner" of any <code>Signal</code> case class declared within
     * this <code>SignalEmitter</code> 
     */
    implicit protected object owner extends SignalOwner
    
    /**
     * Emits a <code>Signal</code> declared in this <code>SignalEmitter</code>
     * 
     * @param signal the <code>Signal</code> occurrence to be emitted
     * @param m implicit <code>scala.Predef.Manifest</code> corresponding to
     * the passed-in <code>Signal</code> instance, allowing to retrieve all the 
     * connections associated with the signal
     * @return true if succeeded, false otherwise
     */
    def emit[T<:Signal](signal:T)(implicit m: Manifest[T]):Boolean = {
      emitSignal(signal, owner)
    }
  }
  
  /**
   * Connects a <code>Signal</code> to some behavior using a 
   * partial function on the concerned <code>Signal</code>.
   * 
   * @param f partial function on the <code>Signal</code> to be connected
   * @param m implicit <code>scala.Predef.Manifest</code> corresponding to
   * the passed-in <code>Signal</code> instance, allowing to retrieve all the 
   * connections associated with the involved signal
   * @return the connection i.e. <code>Some(f)</code> if succeeded, </code>None<code> otherwise
   */
  def connect[T<:Signal](f : PartialFunction[T, _])(implicit m: Manifest[T]): Option[PartialFunction[T, _]] = {
    try {
      // add the connection to the centralized connectionMap
      connectionMap = connectionMap(m.toString) = (f :: connectionMap(m.toString))
      Some(f)
    }
    catch {
      case e:NoSuchElementException =>
        // create a new connection list
        connectionMap = connectionMap(m.toString) = List(f)
        Some(f)
      case _ => None
    }
  }
  
  /**
   * Removes an active connection .
   * 
   * @param of the connection as returned by the <code>connect</code> method
   * @param m implicit <code>scala.Predef.Manifest</code> corresponding to
   * the passed-in <code>Signal</code> instance, allowing to retrieve all the 
   * connections associated with the involved signal
   * @return the connection i.e. <code>Some(f)</code> if succeeded, </code>None<code> otherwise
   */
  def disconnect[T<:Signal](of: Option[PartialFunction[T, _]])(implicit m: Manifest[T]) = {
    of match {
      case Some(f) =>
        try {
          // removes the list item
          connectionMap = connectionMap(m.toString) = connectionMap(m.toString).filter{_==f}
        }
        catch {
          case _ => //can't disconnect
        }
      case _ => //nothing to do
    }
  }
  
  /**
   * Emits a <code>Signal</code>.
   * 
   * @param signal the <code>Signal>/code> to be emitted
   * @param owner the required "Secret owner" of the <code>Signal</code>
   * @param m implicit <code>scala.Predef.Manifest</code> corresponding to
   * the passed-in <code>Signal</code> instance, allowing to retrieve all the 
   * connections associated with the involved signal
   * @return true if succeeded, false otherwise
   */
  def emitSignal[T<:Signal](signal:T, owner:SignalOwner)(implicit m: Manifest[T]):Boolean = {
    // controls the owner
    if (signal.getOwner == owner) {
      // delegates to the dispatcher Actor
      dispatcher ! (m.toString, signal)
      true
    }
    else
      false; //TODO throw an exception instead
  }
  
  /**
   * Stops the dispatcher. This method must be invoked with care because, as soon as
   * invoked, the overall <code>signal</code> component gets inactive.
   */
  def stopDispatcher() {
    dispatcher ! Stop
  }
}

/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.mutable.Queue
import akka.actor.PoisonPill

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply
}


class BinaryTreeSet extends Actor with Stash {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => {
      //System.out.println("tree: " + op)
      root ! op
    }
    case GC => {
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    }
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC =>  //ignore
    case CopyFinished => {
      root ! PoisonPill
      //context.stop(root)
      Thread.sleep(400)
      
//      pendingQueue.foreach { newRoot ! _ }
//      pendingQueue = Queue.empty[Operation]
      
      root = newRoot
      unstashAll()
      context become normal
    }
    case op: Operation => stash() //pendingQueue.enqueue(op)
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case msg @ Insert(requester, id, msgElem) => {
      if (elem == msgElem) {
        removed = false
        requester ! OperationFinished(id)
      }
      else {
        //System.out.println("node insert: " + newNode)
        println("created " + msgElem)
        if (elem > msgElem) {
          val left = subtrees.getOrElse(Left, null)
          if (left == null) {
            subtrees = subtrees.updated(Left, context.actorOf(props(msgElem, false)));
            requester ! OperationFinished(id)
          }
          else left ! msg
        }
        else {
          val right = subtrees.getOrElse(Right, null)
          if(right == null) {
            subtrees = subtrees.updated(Right, context.actorOf(props(msgElem, false)));
            requester ! OperationFinished(id)
          }
          else right ! msg    
        }
      }
    }
    case msg @ Contains(requester, id, msgElem) => {
      //System.out.println("node contains: " + self)
      if (elem == msgElem) requester ! ContainsResult(id, !removed)
      else if (elem > msg.elem) {
        val left = subtrees.getOrElse(Left, null)
        if (left == null) requester ! ContainsResult(id, false)
        else left ! msg
      } else {
        val right = subtrees.getOrElse(Right, null)
        if (right == null) requester ! ContainsResult(id, false)
        else right ! msg
      }
    }
    case msg @ Remove(requester, id, msgElem) => {
      if (elem == msgElem) {
        removed = true
        requester ! OperationFinished(id)
      } else if (elem > msgElem) {
        val left = subtrees.getOrElse(Left, null)
        if (left == null) requester ! OperationFinished(id)
        else left ! msg
      } else {
        val right = subtrees.getOrElse(Right, null)
        if (right == null) requester ! OperationFinished(id)
        else right ! msg
      }
    }
    case CopyTo(newRoot) => {
      if (removed && subtrees.isEmpty) {
        context.parent ! CopyFinished
      } else {
        if (!removed) newRoot ! Insert(self, -1, elem)
        
        val children = subtrees.values
        children.foreach { _ ! CopyTo(newRoot) }
        context.become(copying(children.toSet, removed))
      }
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(-1) => {
      if (expected.isEmpty) context.parent ! CopyFinished
      else context.become(copying(expected, true))
    }
    case CopyFinished => {
      println("terminated " + elem)
      self ! PoisonPill
      val newExpected = expected - sender
      if (newExpected.isEmpty && insertConfirmed) {
        //subtrees.values.foreach { _ ! PoisonPill }
       // Thread.sleep(400)
        context.parent ! CopyFinished
      } else {
        context.become(copying(newExpected, insertConfirmed))
      }
    }
  }
}
